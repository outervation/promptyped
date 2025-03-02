{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Engine where

import BuildSystem as BS
import Control.Monad.Except
import Core
import Data.Aeson as AE
import Data.Aeson.Types qualified as AET
import Data.ByteString.Lazy qualified as LBS
import Data.List.Extra (takeEnd)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Logging qualified
import OpenRouterClient as Client
import Relude
import Tools qualified

shortenedMessageLength :: Int
shortenedMessageLength = 200

numRecentAiMessagesNotToTruncate :: Int
numRecentAiMessagesNotToTruncate = 2

numOldMessagesToKeepInContext :: Int
numOldMessagesToKeepInContext = 10

updateStats :: GenerationStats -> UsageData -> Int64 -> AppM ()
updateStats generation usage timeTaken = do
  let ifNZOr x y = if x /= 0 then x else y
  let metrics =
        mempty
          { metricsTokensIn = ifNZOr (tokens_prompt generation) (prompt_tokens usage),
            metricsTokensOut = ifNZOr (tokens_completion generation) (completion_tokens usage),
            metricsCost = total_cost generation,
            metricsApiTime = timeTaken
          }
  liftIO $ Logging.logInfo "QueryMetrics" (show metrics)
  modify' $ updateStateMetrics metrics

runPrompt :: [Message] -> AppM Message
runPrompt messages = do
  st <- get
  liftIO $ Logging.logInfo "SendMessages" (show . reverse . take 5 $ reverse messages)
  let openFileNames = T.intercalate "," $ map openFileName (stateOpenFiles st)
      existingFileNames = T.intercalate "," $ map existingFileName (stateFiles st)
  liftIO $ Logging.logInfo "SendMessages" $ "OpenFiles: " <> openFileNames
  liftIO $ Logging.logInfo "SendMessages" $ "ExistingFiles: " <> existingFileNames
  cfg <- ask
  let timedQuery = timeIONano64 $ Client.sendQuery (configApiSite cfg) (configApiKey cfg) "prompTyped" "prompTyped" (configModel cfg) (configModelTemperature cfg) messages
  (mayRes, nanosTaken) <- liftIO timedQuery
  case mayRes of
    Left err -> throwError $ "Error running prompt, with messages " <> show messages <> ": " <> err
    Right queryResult -> updateStats (stats queryResult) (usage queryResult) nanosTaken >> pure (message queryResult)

makeSyntaxErrorCorrectionPrompt :: (ToJSON a) => [Tools.Tool] -> a -> Message -> Text -> [Message]
makeSyntaxErrorCorrectionPrompt tools exampleReturn llmMsg err = do
  let returnValueDesc = Tools.returnValueToDescription exampleReturn
      toolDesc = Tools.toolsToDescription tools
      msgBeginning = "You are an agent responsible for error correction of LLM output. There is a specific syntax for tools that the LLM could use, described as follows: " <> toolDesc <> "\nThere is also a syntax for values the LLM may return, as follows: " <> returnValueDesc
      msgRes = "The LLM returned syntactically incorrect output:\n" <> (content llmMsg) <> "\nThe exact error was: " <> err <> "\nPlease output the same output as above but with the syntax error corrected, thanks!"
  return $ Message (roleName RoleUser) (msgBeginning <> msgRes)
  
runPromptWithSyntaxErrorCorrection :: (ToJSON a) => [Tools.Tool] -> a -> [Message] -> AppM Message
runPromptWithSyntaxErrorCorrection tools example messages = do
  res <- runPrompt messages
  let mayToolsCalled = Tools.findToolsCalled (content res) tools
  case mayToolsCalled of
    Right _ -> pure res
    Left err -> do
      let errorCorrectionMsg = makeSyntaxErrorCorrectionPrompt tools example res err
      res' <- runPrompt errorCorrectionMsg
      let mayToolsCalled' = Tools.findToolsCalled (content res') tools
      case mayToolsCalled' of
        Right _ -> pure res'
        Left _ -> pure res

mergeAdjacentRoleMessages :: [Message] -> [Message]
mergeAdjacentRoleMessages [] = []
mergeAdjacentRoleMessages [msg] = [msg]
mergeAdjacentRoleMessages (msg1:msg2:rest)
  | role msg1 == role msg2 =
      let mergedContent = content msg1 <> "\n (...consecutive messages from same role merged...) \n" <> content msg2
          mergedMsg = msg1 { content = mergedContent }
      in mergeAdjacentRoleMessages (mergedMsg:rest)
  | otherwise = msg1 : mergeAdjacentRoleMessages (msg2:rest)

contextToMessages :: (ToJSON a) => Context -> [Tools.Tool] -> AppState -> a -> [Message]
contextToMessages Context {..} tools theState exampleReturn = do
  let messages = map snd contextRest
      returnValueDesc = Tools.returnValueToDescription exampleReturn
      allTexts = [contextBackground, filesDesc, openFilesDesc, toolDesc, returnValueDesc, "YOUR CURRENT TASK: " <> contextTask]
   in mergeAdjacentRoleMessages $ Message {role = roleName RoleUser, content = T.unlines allTexts} : messages
  where
    toolDesc = Tools.toolsToDescription tools
    openFilesDesc = "All currently open files: \n " <> unlines (map renderOpenFile $ stateOpenFiles theState)
    filesDesc = "All available files: \n " <> unlines (map show $ stateFiles theState)

shortenOldErrorMessages :: [(MsgKind, Message)] -> [(MsgKind, Message)]
shortenOldErrorMessages msgs =
  case lastErrorIndex of
    Nothing -> msgs
    Just _ -> zipWith shorten [0 ..] msgs
  where
    -- Helper predicate to tell whether a message is an error.
    isError :: MsgKind -> Bool
    isError CompileFailMsg = True
    isError TestFailMsg = True
    isError _ = False

    -- Get the indices of all error messages in the list.
    errorIndices :: [Int]
    errorIndices = [i | (i, (mk, _)) <- zip [0 ..] msgs, isError mk]

    -- Find the index of the last error message, if there is one.
    lastErrorIndex :: Maybe Int
    lastErrorIndex = viaNonEmpty last errorIndices

    -- If this is an error message and it is not the last one,
    -- shorten its content to the first shortenedMessageLength characters.
    shorten :: Int -> (MsgKind, Message) -> (MsgKind, Message)
    shorten i (mk, msg)
      | isError mk && Just i /= lastErrorIndex =
          (mk, msg {content = T.take shortenedMessageLength (content msg <> "... (truncated)")})
      | otherwise = (mk, msg)

truncateOldMessages :: Text -> Int -> Int -> [(MsgKind, Message)] -> [(MsgKind, Message)]
truncateOldMessages role numRecentMessagesToKeep numCharsToKeep msgs =
  let recentIndices =
        map fst
          $ take numRecentMessagesToKeep
          $ reverse [(i, m) | (i, m) <- zip ([0 ..] :: [Int]) msgs, (_, msg) <- [m], msg.role == role]
   in zipWith
        ( curry
            ( \(i, (kind, msg)) ->
                if msg.role == role && i `notElem` recentIndices
                  then (kind, msg {content = T.take numCharsToKeep msg.content <> "... (truncated)"})
                  else (kind, msg)
            )
        )
        [0 ..]
        msgs

data ErrorKind = SyntaxError | SemanticError
  deriving (Eq, Ord, Show)

runAiFunc ::
  forall bs a b.
  (FromJSON a, ToJSON a, Show a, BS.BuildSystem bs) =>
  Context ->
  [Tools.Tool] ->
  a ->
  (a -> AppM (Either (MsgKind, Text) b)) ->
  RemainingFailureTolerance ->
  AppM b
runAiFunc origCtxt tools exampleReturn postProcessor remainingErrs = do
  when (remainingErrs <= 0) $ throwError "Aborting as reached max number of errors"
  theState <- get
  let truncateOldAiMessages = truncateOldMessages "assistant" numRecentAiMessagesNotToTruncate shortenedMessageLength
      aiTruncatedCtxt = updateContextMessages origCtxt truncateOldAiMessages
      shortenedOldErrCtxt = updateContextMessages aiTruncatedCtxt shortenOldErrorMessages
      ctxt = updateContextMessages shortenedOldErrCtxt (takeEnd (numOldMessagesToKeepInContext + 1))
      messages = contextToMessages ctxt tools theState exampleReturn
  res <- runPrompt messages
  liftIO $ Logging.logInfo "AiResponse" (show res)
  let aiMsg = content res
      mayToolsCalled = Tools.findToolsCalled (content res) tools
      mayRawTextBlocks = Tools.extractRawStrings (content res)
      ctxtWithAiMsg = addToContextAi ctxt OtherMsg aiMsg
  case (mayToolsCalled, mayRawTextBlocks) of
    (Left err, _) -> addErrorAndRecurse ("Error in function calls/return: " <> err) ctxtWithAiMsg SyntaxError OtherMsg
    (Right [], _) -> addErrorAndRecurse "Must call a tool or return. Remember the syntax is ToolName=<[{ someJson }]> , not ToolName=[{ someJson }] and not ToolName<[{ someJson }]> (replace ToolName here with the actual name of the tool; ToolName itself is not a tool!)." ctxtWithAiMsg SyntaxError OtherMsg
    (_, Left err) -> addErrorAndRecurse ("Error in raw text syntax: " <> err) ctxtWithAiMsg SyntaxError OtherMsg
    (Right callsRaw, Right rawTextBlocks) -> handleToolCalls ctxtWithAiMsg callsRaw rawTextBlocks
  where
    recur recurCtxt remainingErrs' = runAiFunc @bs recurCtxt tools exampleReturn postProcessor remainingErrs'

    handleToolCalls :: Context -> [(Tools.Tool, [AET.Object])] -> Tools.RawTexts -> AppM b
    handleToolCalls ctxtWithAiMsg callsRaw rawTextBlocks = case Tools.processToolsArgs callsRaw rawTextBlocks of
      Left err -> addErrorAndRecurse ("Error in function calls/return logic: " <> err) ctxtWithAiMsg SyntaxError OtherMsg
      Right calls -> do
        let ctxtUpdates = flip map calls $ \x innerCtxt -> Tools.runTool @bs rawTextBlocks x innerCtxt
        finalCtxt <- foldlM (\acc f -> f acc) ctxtWithAiMsg ctxtUpdates
        cfg <- ask
        case Tools.getReturn calls of
          Just ret -> postProcessor ret >>= either (handleReturnError finalCtxt) pure
          Nothing -> recur finalCtxt (configTaskMaxFailures cfg)

    addErrorAndRecurse errMsg theCtxt errKind msgKind = do
      liftIO $ Logging.logWarn "RunAiFunc" errMsg
      putTextLn $ "Encountered error: " <> errMsg
      cfg <- ask
      let ctxt' = addErrorToContext theCtxt errMsg msgKind
      when (errKind == SyntaxError) $ modify $ updateStateMetrics (mempty {metricsNumSyntaxErrors = 1})
      let newErrs = if errKind == SyntaxError then remainingErrs - 1 else configTaskMaxFailures cfg
      recur ctxt' newErrs

    addErrorToContext :: Context -> Text -> MsgKind -> Context
    addErrorToContext theCtx err msgKind =
      addToContext theCtx msgKind Message {role = roleName RoleUser, content = err}

    handleReturnError :: Context -> (MsgKind, Text) -> AppM b
    handleReturnError theCtxt (kind, err) =
      addErrorAndRecurse ("Error with return value: " <> err) theCtxt SemanticError kind

checkReturnType :: (FromJSON a, ToJSON a) => a -> AET.Object -> Either Text a
checkReturnType referenceObj obj = do
  let objJson = encode obj
  let refJson = encode referenceObj
  let refJsonText = TE.decodeUtf8Lenient $ LBS.toStrict refJson
  first (\err -> "Error in return value json format, should match " <> refJsonText <> "; error is " <> T.pack err) (eitherDecode objJson)
