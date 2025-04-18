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
import FileSystem qualified as FS
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

numSyntaxRejectsToCauseContextReset :: Int
numSyntaxRejectsToCauseContextReset = 5

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

runPrompt :: IntelligenceRequired -> [Message] -> AppM (Message, UsageData)
runPrompt intReq messages = do
  st <- get
  cfg <- ask
  liftIO $ Logging.logInfo "SendMessages" (T.unlines . map renderMessage . reverse . take 5 $ reverse messages)
  let openFileNames = T.intercalate "," $ map openFileName (stateOpenFiles st)
      existingFileNames = T.intercalate "," $ map existingFileName (stateFiles st)
      model = getModel cfg intReq
  liftIO $ Logging.logInfo "SendMessages" $ "OpenFiles: " <> openFileNames
  liftIO $ Logging.logInfo "SendMessages" $ "ExistingFiles: " <> existingFileNames

  let timedQuery = timeIONano64 $ Client.sendQuery (configApiSite cfg) (configApiKey cfg) "prompTyped" "prompTyped" model (configModelTemperature cfg) messages
  (mayRes, nanosTaken) <- liftIO timedQuery
  case mayRes of
    Left err -> throwError $ "Error running prompt, with messages " <> show messages <> ": " <> err
    Right queryResult -> updateStats (stats queryResult) (usage queryResult) nanosTaken >> pure (message queryResult, usage queryResult)

makeSyntaxErrorCorrectionPrompt :: (ToJSON a) => [Tools.Tool] -> a -> Message -> Text -> [Message]
makeSyntaxErrorCorrectionPrompt tools exampleReturn llmMsg err = do
  let returnValueDesc = Tools.returnValueToDescription exampleReturn
      toolDesc = Tools.toolsToDescription tools
      msgBeginning = "You are an agent responsible for error correction of LLM output. There is a specific syntax for tools that the LLM could use, described as follows: " <> toolDesc <> "\nThere is also a syntax for values the LLM may return, as follows: " <> returnValueDesc
      msgRes = "The LLM returned syntactically incorrect output:\n" <> content llmMsg <> "\nThe exact error was: " <> err <> "\nPlease output the same output as above but with the syntax error corrected, thanks! If you can't fix it, just return it as-is and it'll be handled downstream, don't panic."
  return $ Message (roleName RoleUser) (msgBeginning <> msgRes)

runPromptWithSyntaxErrorCorrection :: (ToJSON a) => IntelligenceRequired -> [Tools.Tool] -> a -> [Message] -> AppM (Message, UsageData)
runPromptWithSyntaxErrorCorrection intReq tools example messages = do
  (res, queryStats) <- runPrompt intReq messages
  let mayToolsCalled = Tools.findToolsCalled (content res) tools
      mayRawTextBlocks = Tools.extractRawStrings (content res)
  case (mayToolsCalled, mayRawTextBlocks, T.length (content res) == 0) of
    (_, _, True) -> pure (res, queryStats)
    (Right [(Tools.ToolReturn, _)], Right (_ : _), False) -> handleErr "Raw text block seen but no tool call using it" res queryStats
    (Right [], Right (_ : _), False) -> handleErr "Raw text block seen but no tool calls that could use it" res queryStats
    (Right _, Right _, False) -> pure (res, queryStats)
    (Left err, _, False) -> handleErr err res queryStats
    (_, Left err, False) -> handleErr err res queryStats
  where
    handleErr err res queryStats = do
      liftIO $ Logging.logInfo "RunPromptWithSyntaxErrorCorrection" "Requesting syntax fix."
      let errorCorrectionMsg = makeSyntaxErrorCorrectionPrompt tools example res err
      (res', _) <- runPrompt intReq errorCorrectionMsg
      let mayToolsCalled' = Tools.findToolsCalled (content res') tools
      case mayToolsCalled' of
        Right _ -> pure (res', queryStats)
        Left _ -> pure (res, queryStats)

mergeAdjacentRoleMessages :: [Message] -> [Message]
mergeAdjacentRoleMessages [] = []
mergeAdjacentRoleMessages [msg] = [msg]
mergeAdjacentRoleMessages (msg1 : msg2 : rest)
  | role msg1 == role msg2 =
      let mergedContent = content msg1 <> "\n (...consecutive messages from same role merged...) \n" <> content msg2
          mergedMsg = msg1 {content = mergedContent}
       in mergeAdjacentRoleMessages (mergedMsg : rest)
  | otherwise = msg1 : mergeAdjacentRoleMessages (msg2 : rest)

getTask :: AppState -> IsCloseFileTask -> Text -> Text
getTask st isCloseFileTask mainTask = do
  let res = stateCompileTestRes st
  "YOUR CURRENT TASK: " <> case (compileRes res, testRes res, isCloseFileTask) of
    (_, _, IsCloseFileTaskTrue) -> "Please close the least important open file, to free up space in the context. The task you were working on when the context got too large is as follows; you should close the file least relevant to it: " <> mainTask
    (Nothing, Nothing, IsCloseFileTaskFalse) -> mainTask
    (Just compileErr, _, IsCloseFileTaskFalse) -> "Fix the project build error. The error: \n" <> compileErr <> "\n. The task you were working on when compilation failed:  " <> mainTask
    (Nothing, Just testErr, IsCloseFileTaskFalse) -> "Fix the error that occurred building or running the tests. The error: \n" <> testErr <> "\n. The task you were working on when compilation failed: " <> mainTask

contextToMessages :: forall bs a. (BS.BuildSystem bs, ToJSON a) => Context -> [Tools.Tool] -> AppState -> IsCloseFileTask -> a -> AppM [Message]
contextToMessages Context {..} tools theState isCloseFileTask exampleReturn = do
  openFilesDesc <- getOpenFilesDesc
  let messagesToUse = takeEnd (numOldMessagesToKeepInContext + 1) contextRest
      messages = map snd messagesToUse
      taskDesc = getTask theState isCloseFileTask contextTask
      returnValueDesc = Tools.returnValueToDescription exampleReturn
      allTexts = [contextBackground, filesDesc, openFilesDesc, toolDesc, returnValueDesc, taskDesc]
   in pure $ mergeAdjacentRoleMessages $ Message {role = roleName RoleUser, content = T.unlines allTexts} : messages
  where
    toolDesc = Tools.toolsToDescription tools
    render :: FileFocused -> Text -> AppM Text
    render isFocused file = do
      isSourceFile <- isBuildableFile @bs file
      if (not isSourceFile) || isFocused == FileFocusedTrue
        then pure $ (FS.addTenthLineNumbersToText file)
        else pure file
    getOpenFilesDesc :: AppM Text
    getOpenFilesDesc = do
      renderedFiles <- mapM (renderOpenFile render) $ stateOpenFiles theState
      pure $ "All currently open files: \n " <> unlines renderedFiles
    filesDesc = "All available files: \n " <> unlines (map renderExistingFile $ stateFiles theState)

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

data FileClosed = FileClosed
  { fileName :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON FileClosed

instance FromJSON FileClosed

combineValidators ::
  forall a b.
  (Show b) =>
  (Context -> a -> AppM (Either (MsgKind, Text) ())) ->
  (Context -> a -> AppM (Either (MsgKind, Text) b)) ->
  (Context -> a -> AppM (Either (MsgKind, Text) b))
combineValidators validator1 validator2 = \ctxt x -> do
  let handleSecondValidator _ = validator2 ctxt x
  res1 <- validator1 ctxt x
  eitherM (pure . Left) handleSecondValidator res1

combineValidatorsSameRes ::
  forall a b.
  (Show b, Eq a, Eq b) =>
  (Context -> a -> AppM (Either (MsgKind, Text) b)) ->
  (Context -> a -> AppM (Either (MsgKind, Text) b)) ->
  (Context -> a -> AppM (Either (MsgKind, Text) b))
combineValidatorsSameRes validator1 validator2 = \ctxt x -> do
  let validate val1 val2 = do
        when (val1 /= val2) $ throwError $ "Different results returned from validators: " <> show val1 <> " vs " <> show val2
        return $ Right val2
      handleSecondValidator val1 = do
        res2 <- validator2 ctxt x
        eitherM (pure . Left) (validate val1) res2
  res1 <- validator1 ctxt x
  eitherM (pure . Left) handleSecondValidator res1

validateFileClosed :: Context -> FileClosed -> AppM (Either (MsgKind, Text) FileClosed)
validateFileClosed ctxt fc@(FileClosed fileName) = do
  case hasAnyFileBeenClosed ctxt of
    True -> pure $ Right fc
    False -> do
      st <- get
      case fileAlreadyOpen fileName st of
        False -> pure $ Right fc
        True -> pure $ Left (OtherMsg, "Error: claimed to have closed file " <> fileName <> " but no file has been closed.")

data IsCloseFileTask = IsCloseFileTaskTrue | IsCloseFileTaskFalse
  deriving (Eq, Ord, Show)

runAiFunc ::
  forall bs a b.
  (FromJSON a, ToJSON a, Show a, BS.BuildSystem bs) =>
  Context ->
  IntelligenceRequired ->
  [Tools.Tool] ->
  a ->
  (Context -> a -> AppM (Either (MsgKind, Text) b)) ->
  RemainingFailureTolerance ->
  AppM b
runAiFunc = runAiFuncInner @bs IsCloseFileTaskFalse

runAiFuncInner ::
  forall bs a b.
  (FromJSON a, ToJSON a, Show a, BS.BuildSystem bs) =>
  IsCloseFileTask ->
  Context ->
  IntelligenceRequired ->
  [Tools.Tool] ->
  a ->
  (Context -> a -> AppM (Either (MsgKind, Text) b)) ->
  RemainingFailureTolerance ->
  AppM b
runAiFuncInner isCloseFileTask origCtxt intReq tools exampleReturn postProcessor remainingErrs = do
  when (remainingErrs <= 0) $ throwError "Aborting as reached max number of errors"
  cfg <- ask
  theState <- get
  let (RemainingFailureTolerance failureToleranceInt) = configTaskMaxFailures cfg
      -- when (theState.stateCompileTestRes.numConsecutiveSyntaxCheckFails > failureToleranceInt) $ throwError "Aborting as reached max number of errors attempting to write syntactically correct code"
      shouldClearMessages = theState.stateCompileTestRes.numConsecutiveSyntaxCheckFails > failureToleranceInt
      origCtxt' = if shouldClearMessages then updateContextMessages origCtxt (const []) else origCtxt
      truncateOldAiMessages = truncateOldMessages "assistant" numRecentAiMessagesNotToTruncate shortenedMessageLength
      aiTruncatedCtxt = updateContextMessages origCtxt' truncateOldAiMessages
      ctxt = updateContextMessages aiTruncatedCtxt shortenOldErrorMessages
  messages <- contextToMessages @bs ctxt tools theState isCloseFileTask exampleReturn
  (res, queryStats) <- runPromptWithSyntaxErrorCorrection intReq tools exampleReturn messages
  when (prompt_tokens queryStats > configModelMaxInputTokens cfg && isCloseFileTask == IsCloseFileTaskFalse) $ do
    st <- get
    let msg = "Input context length is now " <> show (prompt_tokens queryStats) <> ", more than the configured max of " <> show (configModelMaxInputTokens cfg) <> ". Please CloseFile the least important open file."
        fileClosedRes = FileClosed "leastImportantFileName.go"
        fileClosedExample = Tools.returnValueToDescription $ fileClosedRes
        openFileNames :: [Text]
        openFileNames = map (show . openFileName) $ stateOpenFiles st
        taskExtra = "Your context is now too large, so please CloseFile the least important file (based on the below task) and after calling CloseFile immediately return like " <> fileClosedExample <> ". Please don't use any other tools or return anything else as they're disabled until you return a FileClosed. The open files (which you may close) are: " <> (T.intercalate ", " openFileNames)
        baseCtxt = makeBaseContext (contextBackground ctxt) (taskExtra <> "\nThe task you were working on: \n" <> contextTask ctxt)
        fileCloseContext = (addErrorToContext baseCtxt msg OtherMsg)
        maxErrs = configTaskMaxFailures cfg
    liftIO $ Logging.logInfo "ContextReduction" $ "Telling model to reduce context size; msg: " <> msg
    closedFile <- runAiFuncInner @bs IsCloseFileTaskTrue fileCloseContext MediumIntelligenceRequired [Tools.ToolCloseFile, Tools.ToolReturn] fileClosedRes validateFileClosed maxErrs
    liftIO $ Logging.logInfo "ContextReduction" $ "Successfully closed file: " <> show closedFile
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
    recur recurCtxt remainingErrs' = runAiFuncInner @bs isCloseFileTask recurCtxt intReq tools exampleReturn postProcessor remainingErrs'

    handleToolCalls :: Context -> [(Tools.Tool, [AET.Object])] -> Tools.RawTexts -> AppM b
    handleToolCalls ctxtWithAiMsg callsRaw rawTextBlocks =
      Tools.processToolsArgs callsRaw rawTextBlocks >>= \case
        (errs, []) -> addErrorAndRecurse ("Error in function calls/return logic: " <> T.intercalate "," errs) ctxtWithAiMsg SyntaxError OtherMsg
        (errs, calls) -> do
          liftIO $ Logging.logInfo "RunAiFunc" $ "Got " <> (show $ length errs) <> " errors, and func calls: " <> (T.intercalate ", " $ map (Tools.toolName . Tools.toolCallTool) calls)
          cfg <- ask
          let toolProcessingCtxt = foldr (\err innerCtxt -> addErrorToContext innerCtxt err OtherMsg) ctxtWithAiMsg errs
              ctxtUpdates = flip map calls $ \x innerCtxt -> Tools.runTool @bs rawTextBlocks x innerCtxt
          finalCtxt <- foldlM (\acc f -> f acc) toolProcessingCtxt ctxtUpdates
          let numNewErrs = contextNumErrors finalCtxt - contextNumErrors toolProcessingCtxt
          case (Tools.getReturn calls, numNewErrs + length errs > 0) of
            (Just ret, False) -> postProcessor finalCtxt ret >>= either (handleReturnError finalCtxt) pure
            _ -> recur finalCtxt (configTaskMaxFailures cfg)

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
