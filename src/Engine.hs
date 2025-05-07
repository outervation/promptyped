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
import Data.Set qualified as Set
import FileSystem qualified as FS
import Logging qualified
import OpenRouterClient as Client
import Relude
import Tools qualified

allTools :: [Tools.Tool]
allTools = [Tools.ToolOpenFile, Tools.ToolFocusFile, Tools.ToolCloseFile, Tools.ToolAppendFile, Tools.ToolInsertInFile, Tools.ToolEditFileByMatch, Tools.ToolAddDependency, Tools.ToolSummariseAction, Tools.ToolPanic, Tools.ToolReturn]

readOnlyTools :: [Tools.Tool]
readOnlyTools = [Tools.ToolOpenFile, Tools.ToolFocusFile, Tools.ToolCloseFile, Tools.ToolSummariseAction, Tools.ToolPanic, Tools.ToolReturn]


shortenedMessageLength :: Int
shortenedMessageLength = 200

numRecentAiMessagesNotToTruncate :: Int
numRecentAiMessagesNotToTruncate = 1

numOldMessagesToKeepInContext :: Int
numOldMessagesToKeepInContext = 10

numSyntaxRejectsToCauseContextReset :: Int
numSyntaxRejectsToCauseContextReset = 5

maxConsecutiveCompileErrors :: Int
maxConsecutiveCompileErrors = 5

numRecentEventsToShow :: Int
numRecentEventsToShow = 100

data CompileProgressResult = CompileProgressResult
  { progressMade :: Bool,
        rationale :: Text
  }
    deriving (Generic, Eq, Ord, Show)

instance ToJSON CompileProgressResult
instance FromJSON CompileProgressResult

data RelevantFilesResult = RelevantFilesResult
  { relevantFiles :: [Text]
  }
    deriving (Generic, Eq, Ord, Show)

instance ToJSON RelevantFilesResult
instance FromJSON RelevantFilesResult

data CompileSolutionResult = CompileSolutionResult
  { hasSolution :: Bool,
        compileErrorSolution :: Text
  }
    deriving (Generic, Eq, Ord, Show)

instance ToJSON CompileSolutionResult
instance FromJSON CompileSolutionResult

getRecentCompileErrors :: [TracedEvent] -> Int -> [Text]
getRecentCompileErrors events n =
  mapMaybe extractError $ takeEnd n events
  where
    extractError :: TracedEvent -> Maybe Text
    extractError (EvtCompileProject (FailedWithPotentiallyVeryLongError (PotentiallyBigMessage msg))) = Just msg
    extractError (EvtCompileProject (FailedWithError msg)) = Just msg -- Also consider regular text errors
    extractError _ = Nothing

validateAlwaysPass :: Context -> a -> AppM (Either (MsgKind, Text) a)
validateAlwaysPass _ x = pure $ Right x

-- Validator for RelevantFilesResult
validateRelevantFilesExist :: Context -> RelevantFilesResult -> AppM (Either (MsgKind, Text) RelevantFilesResult)
validateRelevantFilesExist _ result = do
  st <- get
  let existingFileNames = Set.fromList $ map existingFileName (stateFiles st)
      missingFiles = filter (not . (`Set.member` existingFileNames)) (relevantFiles result)
  if null missingFiles
    then pure $ Right result
    else pure $ Left (OtherMsg, "AI suggested relevant files that do not exist: " <> T.intercalate ", " missingFiles)

handleConsecutiveCompileFailures :: forall bs. (BS.BuildSystem bs) => Context -> AppM Context
handleConsecutiveCompileFailures origCtxt = do
  st <- get
  cfg <- ask
  let consecutiveFails = numConsecutiveCompilationFails (stateCompileTestRes st)
      shouldIntervene = consecutiveFails > 0 && consecutiveFails `mod` maxConsecutiveCompileErrors == 0

  if not shouldIntervene
    then pure origCtxt -- No intervention needed
    else do
      putTextLn $ "Detected " <> show consecutiveFails <> " consecutive compilation failures. Checking for progress..."
      liftIO $ Logging.logWarn "CompileFailureIntervention" $ "Detected " <> show consecutiveFails <> " consecutive failures. Checking progress."

      let recentErrors = getRecentCompileErrors (contextEvents origCtxt) maxConsecutiveCompileErrors
          currentError = fromMaybe "No current error details available" (compileRes $ stateCompileTestRes st)

      -- 1. Check for Progress
      let progressCheckContext = makeBaseContext origCtxt.contextBackground $
            "The last " <> show (length recentErrors) <> " compilation attempts failed. Please analyze the error messages to determine if any progress is being made (e.g., different errors, fewer errors, fixing previous issues).\n" <>
            "Recent Errors (newest last):\n" <> T.unlines (map (\(i :: Int, e) -> T.pack (show i) <> ": " <> truncateText 5 e) $ zip [1..] recentErrors) <> "\n" <>
            "Current Error:\n" <> currentError <> "\n"
          exampleProgress = CompileProgressResult True "Syntax error fixed, now facing a type error."
      (progressResult, _) <- runAiFuncInner @bs IsNestedAiFuncTrue IsCloseFileTaskFalse progressCheckContext LowIntelligenceRequired readOnlyTools exampleProgress validateAlwaysPass (configTaskMaxFailures cfg)

      if progressResult.progressMade
        then do
          putTextLn $ "AI determined progress is being made: " <> progressResult.rationale
          liftIO $ Logging.logInfo "CompileFailureIntervention" $ "Progress detected: " <> progressResult.rationale
          pure origCtxt -- Progress detected, continue normally
        else do
          putTextLn $ "AI determined NO progress is being made: " <> progressResult.rationale <> ". Attempting intervention."
          liftIO $ Logging.logWarn "CompileFailureIntervention" $ "No progress detected: " <> progressResult.rationale <> ". Intervening."

          -- 2. Identify Relevant Files
          allSourceFiles <- filterM (BS.isBuildableFile @bs) $ map existingFileName (stateFiles st)
          modify' clearOpenFiles 
          forM_ allSourceFiles $ \fn -> do
             Tools.openFile @bs Tools.DontFocusOpenedFile fn cfg
          let relevantFilesContext = makeBaseContext origCtxt.contextBackground $
                "Compilation has failed repeatedly without progress. The current error is:\n" <> currentError <> "\n\n" <>
                "The available source files are:\n" <> T.intercalate ", " allSourceFiles <> "\n\n" <>
                "Please identify a small list of file names that are most relevant to understanding and fixing this error (e.g., the file with the error, files defining types/functions mentioned in the error)."
              exampleRelevantFiles = RelevantFilesResult ["main.go", "utils.go"]

          (relevantFilesResult, _) <- runAiFuncInner @bs IsNestedAiFuncTrue IsCloseFileTaskFalse relevantFilesContext HighIntelligenceRequired readOnlyTools exampleRelevantFiles validateRelevantFilesExist (configTaskMaxFailures cfg)

          -- 3. Get Solution Plan (using a temporary context/state view for file focus)
          -- We don't modify the main state here, just the context view for the AI call.
          let filesToFocus = relevantFiles relevantFilesResult
              recentEvents = takeEnd 10 (contextEvents origCtxt) -- Provide recent history

              -- Construct a temporary context view for the solution AI
              solutionPrompt =
                "Compilation is stuck. Current error:\n" <> currentError <> "\n\n" <>
                "Based on the error, the following files were deemed most relevant:\n" <> T.intercalate ", " filesToFocus <> "\n\n" <>
                "Recent actions taken:\n" <> T.unlines (map show recentEvents) <> "\n\n" <>
                "Please analyze the error, relevant files, and recent history to suggest a specific, actionable plan to fix the compilation error. Return=<[{ hasSolution: bool, compileErrorSolution: string }]>. If you cannot find a likely solution, set hasSolution to false."
              solutionContext = makeBaseContext origCtxt.contextBackground solutionPrompt
              exampleSolution = CompileSolutionResult True "Import 'newpkg' in file_a.go and change function signature in file_b.go to accept an integer."

          putTextLn $ "Focusing relevant files for analysis: " <> T.intercalate ", " filesToFocus
          modify' clearOpenFiles -- Clear existing open files first
          forM_ filesToFocus $ \fn -> do
             Tools.openFile @bs Tools.DoFocusOpenedFile fn cfg

          -- Now call the AI with the files focused in the actual state
          (solutionResult, _) <- runAiFuncInner @bs IsNestedAiFuncTrue IsCloseFileTaskFalse solutionContext HighIntelligenceRequired readOnlyTools exampleSolution validateAlwaysPass (configTaskMaxFailures cfg)

          if solutionResult.hasSolution
            then do
              let interventionMessage = "SYSTEM INTERVENTION: Compilation has failed " <> show consecutiveFails <> " times without progress. Based on analysis, a potential solution is:\n" <> solutionResult.compileErrorSolution <> "\nPlease prioritize implementing this approach."
              putTextLn $ "AI suggested a solution: " <> solutionResult.compileErrorSolution
              liftIO $ Logging.logWarn "CompileFailureIntervention" $ "AI suggested solution: " <> solutionResult.compileErrorSolution
              -- Add the intervention message and event to the *original* context
              let updatedCtxt = addEvtToContext (addToContextUser origCtxt OtherMsg interventionMessage) (EvtApproachCorrection solutionResult.compileErrorSolution)
              pure updatedCtxt
            else do
              let abortMsg = "Aborting: Max consecutive compile errors (" <> show consecutiveFails <> ") reached without progress, and AI could not suggest a specific fix."
              putTextLn abortMsg
              liftIO $ Logging.logError "CompileFailureIntervention" abortMsg
              throwError abortMsg

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
  -- liftIO $ Logging.logInfo "SendMessages" (T.unlines . map renderMessage . reverse . take 5 $ reverse messages)
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

makeSyntaxErrorCorrectionPrompt :: (ToJSON a) => [OpenFile] -> [Tools.Tool] -> a -> Message -> Text -> [Message]
makeSyntaxErrorCorrectionPrompt relFiles tools exampleReturn llmMsg err = do
  let returnValueDesc = Tools.returnValueToDescription exampleReturn
      toolDesc = Tools.toolsToDescription tools
      msgBeginning = "You are an agent responsible for error correction of LLM output. There is a specific syntax for tools that the LLM could use, described as follows: " <> toolDesc <> "\nThere is also a syntax for values the LLM may return, as follows: " <> returnValueDesc <> "\n"
      fileTexts = map (\x -> openFileName x <> ": \n" <> FS.addTenthLineNumbersToText (openFileContents x)) relFiles
      openFilesDesc = if null relFiles then "" else "The following open files may be relevant: " <> T.intercalate "\n\n" fileTexts
      msgRes = "The LLM returned syntactically incorrect output:\n" <> content llmMsg <> "\nThe exact error was: " <> err <> "\nPlease output the same output as above but with the syntax error corrected, thanks! For missing textbox names, if a tool call references a text box that doesn't exist, but right after it is an unused textbox that seems to fit, then please fix the textbox name to match. If you can't fix it, just return it as-is and it'll be handled downstream, don't panic."
  return $ Message (roleName RoleUser) (msgBeginning <> openFilesDesc <> msgRes)

runPromptWithSyntaxErrorCorrection :: forall a. (ToJSON a, FromJSON a, Show a) => IntelligenceRequired -> [Tools.Tool] -> a -> [Message] -> AppM (Message, UsageData)
runPromptWithSyntaxErrorCorrection intReq tools example messages = do
  (res, queryStats) <- runPrompt intReq messages
  let mayToolsCalled = Tools.findToolsCalled (content res) tools
      mayRawTextBlocks = Tools.extractRawStrings (content res)
  case (mayToolsCalled, mayRawTextBlocks, T.length (content res) == 0) of
    (_, _, True) -> pure (res, queryStats)
    (Right [], Right [], False) -> pure (res, queryStats)
    (Right [(Tools.ToolReturn, _)], Right (_ : _), False) -> handleErr [] "Raw text block seen but no tool call using it" res queryStats
    (Right [], Right (_ : _), False) -> handleErr [] "Raw text block seen but no tool calls that could use it" res queryStats
    (Right callsRaw, Right rawTextBlocks, False) -> do
      Tools.processToolsArgs @a callsRaw rawTextBlocks >>= \case
        ([], _) -> pure (res, queryStats)
        (errs, _) -> do
          st <- get
          let relFileNames = Tools.getToolCallFileNames callsRaw
              relOpenFiles = mapMaybe (`getOpenFile` st) relFileNames
          handleErr relOpenFiles (T.intercalate ",\n" errs) res queryStats
    (Left err, _, False) -> handleErr [] err res queryStats
    (_, Left err, False) -> handleErr [] err res queryStats
  where
    handleErr relFiles err res queryStats = do
      liftIO $ Logging.logInfo "RunPromptWithSyntaxErrorCorrection" "Requesting syntax fix."
      let errorCorrectionMsg = makeSyntaxErrorCorrectionPrompt relFiles tools example res err
      (res', _) <- runPrompt intReq errorCorrectionMsg
      let mayToolsCalled' = Tools.findToolsCalled (content res') tools
      case mayToolsCalled' of
        Right _ -> do
          liftIO $ Logging.logInfo "RunPromptWithSyntaxErrorCorrection" "Using reponse with syntax fix."
          pure (res', queryStats)
        Left _ -> do
          liftIO $ Logging.logInfo "RunPromptWithSyntaxErrorCorrection" "Using original response."
          pure (res, queryStats)

mergeAdjacentRoleMessages :: [Message] -> [Message]
mergeAdjacentRoleMessages [] = []
mergeAdjacentRoleMessages [msg] = [msg]
mergeAdjacentRoleMessages (msg1 : msg2 : rest)
  | role msg1 == role msg2 =
      let mergedContent = content msg1 <> "\n (...consecutive messages from same role merged...) \n" <> content msg2
          mergedMsg = msg1 {content = mergedContent}
       in mergeAdjacentRoleMessages (mergedMsg : rest)
  | otherwise = msg1 : mergeAdjacentRoleMessages (msg2 : rest)

getTask :: AppState -> IsNestedAiFunc -> IsCloseFileTask -> Text -> Text
getTask st isNestedAiFunc isCloseFileTask mainTask = do
  let res = stateCompileTestRes st
  "YOUR CURRENT TASK: " <> case (compileRes res, testRes res, isCloseFileTask, isNestedAiFunc) of
    (_, _, IsCloseFileTaskTrue, _) -> "Please close the least important open file, to free up space in the context. The task you were working on when the context got too large is as follows; you should close the file least relevant to it: " <> mainTask 
    (Nothing, Nothing, IsCloseFileTaskFalse, _) -> mainTask
    (Just _, _, IsCloseFileTaskFalse, IsNestedAiFuncFalse) -> "Fix the project build error. The error is described above. The task you were working on when compilation failed (don't work on it now, just fix the build):\n\"" <> mainTask <> "\""
    (Nothing, Just _, IsCloseFileTaskFalse, IsNestedAiFuncFalse) -> "Fix the error that occurred building or running the tests. The error is described above. The task you were working on when compilation failed (don't work on it now, just fix the tests):\n\"" <> mainTask <> "\""
    (_, _, IsCloseFileTaskFalse, IsNestedAiFuncTrue) -> mainTask

contextToMessages :: forall bs a. (BS.BuildSystem bs, ToJSON a) => Context -> [Tools.Tool] -> AppState -> IsNestedAiFunc -> IsCloseFileTask -> a -> AppM [Message]
contextToMessages Context {..} tools theState isNestedAiFunc isCloseFileTask exampleReturn = do
  openFilesDesc <- getOpenFilesDesc
  compileTestResDesc <- compileTestDesc $ stateCompileTestRes theState
  let messagesToUse = takeEnd (numOldMessagesToKeepInContext + 1) contextRest
      messages = map snd messagesToUse
      taskDesc = getTask theState isNestedAiFunc isCloseFileTask contextTask
      returnValueDesc = Tools.returnValueToDescription exampleReturn
      allTexts =
        [ contextBackground,
          filesDesc,
          openFilesDesc,
          evtsDesc,
          toolDesc,
          compileTestResDesc,
          taskDesc,
          respReqsDesc,
          returnValueDesc
        ]
   in pure $ mergeAdjacentRoleMessages $ Message {role = roleName RoleUser, content = T.unlines allTexts} : messages
  where
    toolDesc = Tools.toolsToDescription tools
    respReqsDesc = "Your response must either call at least one tool or return a value. If calling any tool, please also include a SummariseAction tool call too, for tracking the intent and future plans."
    evtsDesc = "Here is a list of your recent actions, the intent behind them and their results. Look carefully to identify any circular behaviour here, to avoid getting stuck in a loop, and so you don't lose track of your train of thought/intention:\n" <> T.intercalate "\n" (map show (takeEnd numRecentEventsToShow contextEvents))
    render :: FileFocused -> Text -> AppM Text
    render isFocused file = do
      isSourceFile <- isBuildableFile @bs file
      if (not isSourceFile) || isFocused == FileFocusedTrue
        then pure $ FS.addTenthLineNumbersToText file
        else pure file
    getOpenFilesDesc :: AppM Text
    getOpenFilesDesc = do
      renderedFiles <- mapM (renderOpenFile render) $ stateOpenFiles theState
      pure $ "All currently open files (note these always represent the latest version on disk, even though they may appear in the context before the messages containing changes made to them. Also note that unfocused source files only show datatype and function definitions, not bodies, to save space in context):\n\n" <> unlines renderedFiles
    filesDesc = "All available files: \n " <> unlines (map renderExistingFile $ stateFiles theState)
    compileTestDesc ctRes = do
      let compDesc = case ctRes.compileRes of
            Just err -> "Failed with error: \n" <> err
            Nothing -> "Succeeded!"
          testDesc = case ctRes.testRes of
            Just err -> "Failed with error: \n" <> err
            Nothing -> "Succeeded!"
      return $ "\nLATEST COMPILE/TEST STATE (note this represents the latest results even after messages that many occur below in the context): \nLast compilation: " <> compDesc <> "\nLast unit test run: " <> testDesc <> "\n"

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
combineValidators validator1 validator2 ctxt x = do
  let handleSecondValidator _ = validator2 ctxt x
  res1 <- validator1 ctxt x
  eitherM (pure . Left) handleSecondValidator res1

combineValidatorsSameRes ::
  forall a b.
  (Show b, Eq a, Eq b) =>
  (Context -> a -> AppM (Either (MsgKind, Text) b)) ->
  (Context -> a -> AppM (Either (MsgKind, Text) b)) ->
  (Context -> a -> AppM (Either (MsgKind, Text) b))
combineValidatorsSameRes validator1 validator2 ctxt x = do
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

data IsNestedAiFunc = IsNestedAiFuncTrue | IsNestedAiFuncFalse
  deriving (Eq, Ord, Show)

toolCallsMissingRequiredSummary :: [(Tools.Tool, [AET.Object])]  -> Bool
toolCallsMissingRequiredSummary calls = do
  let tools = map fst calls
  (any Tools.isMutation tools) && Tools.ToolSummariseAction `notElem` tools

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
runAiFunc ctxt intReq tools example postProcessor failureTolerance = do
  (res, _) <- runAiFuncInner @bs IsNestedAiFuncFalse IsCloseFileTaskFalse ctxt intReq tools example postProcessor failureTolerance
  return res

runAiFuncKeepingContext ::
  forall bs a b.
  (FromJSON a, ToJSON a, Show a, BS.BuildSystem bs) =>
  Context ->
  IntelligenceRequired ->
  [Tools.Tool] ->
  a ->
  (Context -> a -> AppM (Either (MsgKind, Text) b)) ->
  RemainingFailureTolerance ->
  AppM (b, Context)
runAiFuncKeepingContext = runAiFuncInner @bs IsNestedAiFuncFalse IsCloseFileTaskFalse 

runAiFuncInner ::
  forall bs a b.
  (FromJSON a, ToJSON a, Show a, BS.BuildSystem bs) =>
  IsNestedAiFunc ->
  IsCloseFileTask ->
  Context ->
  IntelligenceRequired ->
  [Tools.Tool] ->
  a ->
  (Context -> a -> AppM (Either (MsgKind, Text) b)) ->
  RemainingFailureTolerance ->
  AppM (b, Context)
runAiFuncInner isNestedAiFunc isCloseFileTask initialCtxt intReq tools exampleReturn postProcessor remainingErrs = do
  when (remainingErrs <= 0) $ throwError "Aborting as reached max number of errors"
  when (notElem Tools.ToolReturn tools) $ throwError "Missing Return tool!"
  when ((any Tools.isMutation tools) && notElem Tools.ToolSummariseAction tools) $ throwError "Missing SummariseAction tool!"
  origCtxt <- if isNestedAiFunc == IsNestedAiFuncTrue then pure initialCtxt else runActionWithoutModifyingState $ handleConsecutiveCompileFailures @bs initialCtxt
  cfg <- ask
  theState <- get
  let -- (RemainingFailureTolerance failureToleranceInt) = configTaskMaxFailures cfg
      -- when (theState.stateCompileTestRes.numConsecutiveSyntaxCheckFails > failureToleranceInt) $ throwError "Aborting as reached max number of errors attempting to write syntactically correct code"
      shouldClearMessages = theState.stateCompileTestRes.numConsecutiveSyntaxCheckFails > numSyntaxRejectsToCauseContextReset
      origCtxt' = if shouldClearMessages then updateContextMessages origCtxt (const []) else origCtxt
      truncateOldAiMessages = truncateOldMessages "assistant" numRecentAiMessagesNotToTruncate shortenedMessageLength
      aiTruncatedCtxt = updateContextMessages origCtxt' truncateOldAiMessages
      ctxt = updateContextMessages aiTruncatedCtxt shortenOldErrorMessages
  messages <- contextToMessages @bs ctxt tools theState isNestedAiFunc isCloseFileTask exampleReturn
  (res, queryStats) <- runPromptWithSyntaxErrorCorrection intReq tools exampleReturn messages
  when (prompt_tokens queryStats > configModelMaxInputTokens cfg && isCloseFileTask == IsCloseFileTaskFalse) $ do
    st <- get
    let msg = "Input context length is now " <> show (prompt_tokens queryStats) <> ", more than the configured max of " <> show (configModelMaxInputTokens cfg) <> ". Please CloseFile the least important open file."
        fileClosedRes = FileClosed "leastImportantFileName.go"
        fileClosedExample = Tools.returnValueToDescription fileClosedRes
        openFileNames :: [Text]
        openFileNames = map (show . openFileName) $ stateOpenFiles st
        taskExtra = "Your context is now too large, so please CloseFile the least important file (based on the below task) and after calling CloseFile immediately return like " <> fileClosedExample <> ". Please don't use any other tools or return anything else as they're disabled until you return a FileClosed. The open files (which you may close) are: " <> T.intercalate ", " openFileNames
        baseCtxt = makeBaseContext (contextBackground ctxt) (taskExtra <> "\nThe task you were working on: \n" <> contextTask ctxt)
        fileCloseContext = (addErrorToContext baseCtxt msg OtherMsg)
        maxErrs = configTaskMaxFailures cfg
    liftIO $ Logging.logInfo "ContextReduction" $ "Telling model to reduce context size; msg: " <> msg
    closedFile <- runAiFuncInner @bs isNestedAiFunc IsCloseFileTaskTrue fileCloseContext MediumIntelligenceRequired [Tools.ToolCloseFile, Tools.ToolReturn] fileClosedRes validateFileClosed maxErrs
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
    (Right callsRaw, Right _) | toolCallsMissingRequiredSummary callsRaw -> addErrorAndRecurse ("Made state-changing tool calls but didn't include SummariseAction=<[{...}]> call") ctxtWithAiMsg SyntaxError OtherMsg
    (Right callsRaw, Right rawTextBlocks) | otherwise -> handleToolCalls ctxtWithAiMsg callsRaw rawTextBlocks
  where
    recur recurCtxt remainingErrs' = runAiFuncInner @bs isNestedAiFunc isCloseFileTask recurCtxt intReq tools exampleReturn postProcessor remainingErrs'

    handleToolCalls :: Context -> [(Tools.Tool, [AET.Object])] -> Tools.RawTexts -> AppM (b, Context)
    handleToolCalls ctxtWithAiMsg callsRaw rawTextBlocks =
      Tools.processToolsArgs callsRaw rawTextBlocks >>= \case
        (errs, []) -> addErrorAndRecurse ("Error in function calls/return logic: " <> T.intercalate "," errs) ctxtWithAiMsg SyntaxError OtherMsg
        (errs, calls) -> do
          cfg <- ask
          let toolProcessingCtxt = foldr (\err innerCtxt -> addErrorToContext innerCtxt err OtherMsg) ctxtWithAiMsg errs
              ctxtUpdates = flip map calls $ \x innerCtxt -> Tools.runTool @bs rawTextBlocks x innerCtxt
          finalCtxt <- foldlM (\acc f -> f acc) toolProcessingCtxt ctxtUpdates
          let numNewErrs = contextNumErrors finalCtxt - contextNumErrors toolProcessingCtxt
          liftIO $ Logging.logInfo "RunAiFunc" $ "Got errors: " <> show errs <> ",\n " <> show numNewErrs <> " new context errors, and func calls: " <> T.intercalate ", " (map (Tools.toolName . Tools.toolCallTool) calls)
          case (Tools.getReturn calls, numNewErrs + length errs > 0) of
            (Just ret, False) -> do
              liftIO $ Logging.logInfo "RunAiFunc" "Running validator/postprocessor to attempt return"
              let addCtxt addedCtxt x = pure (x, addedCtxt)
              postProcessor finalCtxt ret >>= either (handleReturnError finalCtxt) (addCtxt finalCtxt)
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

    handleReturnError :: Context -> (MsgKind, Text) -> AppM (b, Context)
    handleReturnError theCtxt (kind, err) =
      addErrorAndRecurse ("Error with return value: " <> err) theCtxt SemanticError kind

checkReturnType :: (FromJSON a, ToJSON a) => a -> AET.Object -> Either Text a
checkReturnType referenceObj obj = do
  let objJson = encode obj
  let refJson = encode referenceObj
  let refJsonText = TE.decodeUtf8Lenient $ LBS.toStrict refJson
  first (\err -> "Error in return value json format, should match " <> refJsonText <> "; error is " <> T.pack err) (eitherDecode objJson)
