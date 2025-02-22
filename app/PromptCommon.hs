{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PromptCommon where

import BuildSystem as BS
import Control.Monad.Except
import Core
import Data.Aeson as AE
import Data.Aeson.Types qualified as AET
import Data.ByteString.Lazy qualified as LBS
import Data.Graph
import Data.List.Extra (takeEnd)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import FileSystem qualified as FS
import Logging qualified
import Memoise (memoise)
import OpenRouterClient as Client
import PromptTexts
import Relude
import System.Directory qualified as DIR
import Tools qualified

shortenedMessageLength :: Int
shortenedMessageLength = 200

numRecentAiMessagesNotToTruncate :: Int
numRecentAiMessagesNotToTruncate = 1

numOldMessagesToKeepInContext :: Int
numOldMessagesToKeepInContext = 10

journalFileName :: Text
journalFileName = "journal.txt"

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
  modify' $ updateStateMetrics metrics

runPrompt :: [Message] -> AppM Message
runPrompt messages = do
  st <- get
  liftIO $ Logging.logInfo "SendMessages" (show . reverse . take 5 $ reverse messages)
  let openFileNames = T.intercalate "," $ map openFileName (stateOpenFiles st)
  liftIO $ Logging.logInfo "SendMessages" $ "OpenFiles: " <> openFileNames
  let existingFileNames = T.intercalate "," $ map existingFileName (stateFiles st)
  liftIO $ Logging.logInfo "SendMessages" $ "ExistingFiles: " <> existingFileNames
  cfg <- ask
  let timedQuery = timeIONano64 $ Client.sendQuery (configApiSite cfg) (configApiKey cfg) "prompTyped" "prompTyped" (configModel cfg) messages
  (mayRes, nanosTaken) <- liftIO timedQuery
  case mayRes of
    Left err -> throwError $ "Error running prompt, with messages " <> show messages <> ": " <> err
    Right queryResult -> updateStats (stats queryResult) (usage queryResult) nanosTaken >> pure (message queryResult)

contextToMessages :: (ToJSON a) => Context -> [Tools.Tool] -> AppState -> a -> [Message]
contextToMessages Context {..} tools theState exampleReturn = do
  let messages = map snd contextRest
      returnValueDesc = Tools.returnValueToDescription exampleReturn
      allTexts = [contextBackground, toolDesc, filesDesc, openFilesDesc, returnValueDesc, "YOUR CURRENT TASK: " <> contextTask]
   in Message {role = roleName RoleUser, content = T.unlines allTexts} : messages
  where
    toolDesc = Tools.toolsToDescription tools
    openFilesDesc = "All currently open files: \n " <> unlines (map show $ stateOpenFiles theState)
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
      ctxtWithAiMsg = addToContextAi ctxt OtherMsg aiMsg
  case mayToolsCalled of
    Left err -> addErrorAndRecurse ("Error in function calls/return: " <> err) ctxtWithAiMsg SyntaxError OtherMsg
    Right [] -> addErrorAndRecurse "Must call a tool or return. Remember the syntax is ToolName=<[{ someJson }]> , not ToolName=[{ someJson }] and not ToolName<[{ someJson }]> (replace ToolName here with the actual name of the tool; ToolName itself is not a tool!)." ctxtWithAiMsg SyntaxError OtherMsg
    Right callsRaw -> handleToolCalls ctxtWithAiMsg callsRaw
  where
    recur recurCtxt remainingErrs' = runAiFunc @bs recurCtxt tools exampleReturn postProcessor remainingErrs'

    handleToolCalls :: Context -> [(Tools.Tool, [AET.Object])] -> AppM b
    handleToolCalls ctxtWithAiMsg callsRaw = case Tools.processToolsArgs callsRaw of
      Left err -> addErrorAndRecurse ("Error in function calls/return logic: " <> err) ctxtWithAiMsg SyntaxError OtherMsg
      Right calls -> do
        let ctxtUpdates = flip map calls $ \x innerCtxt -> Tools.runTool @bs x innerCtxt
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

data ThingWithDependencies = ThingWithDependencies
  { name :: Text,
    summary :: Text,
    dependencies :: [Text]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON ThingWithDependencies

instance FromJSON ThingWithDependencies

data ThingsWithDependencies = ThingsWithDependencies
  { items :: [ThingWithDependencies]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON ThingsWithDependencies

instance FromJSON ThingsWithDependencies

topologicalSortThingsWithDependencies :: [ExistingFile] -> ThingsWithDependencies -> Either Text [ThingWithDependencies]
topologicalSortThingsWithDependencies existingFiles (ThingsWithDependencies files) = do
  let existingFileNames = map existingFileName existingFiles
  let isDoc name = isDocFileExtension name -- && elem name existingFileNames
  -- 1. Build a map from fileName -> ThingWithDependencies
  let fileMap :: Map Text ThingWithDependencies
      fileMap = Map.fromList [(f.name, f) | f <- files]

  -- 2. Check every dependency to ensure it exists in fileMap
  forM_ files $ \pf -> do
    forM_ pf.dependencies $ \dep ->
      unless (Map.member dep fileMap || isDoc dep)
        $ Left
        $ "Error: File "
        <> pf.name
        <> " depends on non-mentioned file: "
        <> dep
        <> ". Note that only files we create need to be mentioned, not external libs like Boost etc."

  -- 3. Prepare triples for graphFromEdges:
  --      (the actual node, this node's key, this node's adjacent keys)
  let triples =
        [ ( f,
            f.name,
            f.dependencies -- edges: f -> each of its dependencies
          )
        | f <- files
        ]

  -- Build the graph from these triples
  let (graph, nodeFromVertex, _vertexFromKey) = graphFromEdges triples

  -- 4. Detect cycles using stronglyConnComp
  let sccs = stronglyConnComp triples
  forM_ sccs $ \case
    AcyclicSCC _ -> pure ()
    CyclicSCC cycleGroup ->
      let cycleNames = map (\x -> x.name) cycleGroup
       in Left $ "Error: Cycle detected in file dependencies: " <> show cycleNames

  -- 5. Perform the topological sort
  let sortedVertices = topSort graph

  -- 6. Convert each 'Vertex' back into the original 'ThingWithDependencies'
  let sortedFiles = map (\v -> let (f, _, _) = nodeFromVertex v in f) sortedVertices

  pure $ reverse sortedFiles

validateThingsWithDependencies :: ThingsWithDependencies -> AppM (Either (MsgKind, Text) [ThingWithDependencies])
validateThingsWithDependencies pf = do
  st <- get
  return $ either (\x -> Left (OtherMsg, x)) Right $ topologicalSortThingsWithDependencies (stateFiles st) pf

data ThingWithDescription = ThingWithDescription
  { description :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON ThingWithDescription

instance FromJSON ThingWithDescription

validateAlwaysPass :: a -> AppM (Either (MsgKind, Text) a)
validateAlwaysPass x = pure $ Right x

data CreatedFile = CreatedFile
  { createdFileName :: Text,
    createdFileSummary :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON CreatedFile

instance FromJSON CreatedFile

data CreatedFiles = CreatedFiles
  { createdFiles :: [CreatedFile]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON CreatedFiles

instance FromJSON CreatedFiles

validateCreatedFiles :: CreatedFiles -> AppM (Either (MsgKind, Text) [CreatedFile])
validateCreatedFiles cf = do
  st <- get
  let files = createdFiles cf
  let check x = case fileExists (createdFileName x) st of
        True -> pure $ Right ()
        False -> pure . Left $ "Claimed to have created file " <> show x <> " but it doesn't exist. Did you remember to call AppendFile/OpenFile?"
  liftIO (foldWithErrors check files) >>= either (pure . Left . (OtherMsg,)) (const $ pure $ Right files)

data UnitTest = UnitTest
  { unitTestName :: Text,
    unitTestSummary :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON UnitTest

instance FromJSON UnitTest

data UnitTests = UnitTests
  { unitTestFileName :: Text,
    unitTests :: [UnitTest]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON UnitTests

instance FromJSON UnitTests

validateUnitTests :: UnitTests -> AppM (Either (MsgKind, Text) UnitTests)
validateUnitTests cf = pure $ Right cf

allTools :: [Tools.Tool]
allTools = [Tools.ToolOpenFile, Tools.ToolCloseFile, Tools.ToolAppendFile, Tools.ToolInsertInFile, Tools.ToolEditFile, Tools.ToolPanic, Tools.ToolReturn]

data UnitTestDone = UnitTestDone
  { unitTestPassedSuccessfully :: Bool
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON UnitTestDone

instance FromJSON UnitTestDone

validateUnitTest :: UnitTestDone -> AppM (Either (MsgKind, Text) ())
validateUnitTest t = case unitTestPassedSuccessfully t of
  False -> pure $ Left (OtherMsg, "Your return value of false indicates it didn't pass successfully")
  True -> do
    st <- get
    let res = stateCompileTestRes st
    pure $ case (compileRes res, testRes res) of
      (Nothing, Nothing) -> Right ()
      (Just compileErr, _) -> Left (CompileFailMsg, "Error, compilation failed, fix it before returning. Note that if you see a 'missing import path' compilation error, it may be because you forgot a closing ')' for the go import list. If you see 'is not a package path' when trying to import a local file you created, remember you should include 'project_name/filename', NOT '/home/username/project_name/filename' or 'username/project_name/filename' The last error was: " <> compileErr)
      (Nothing, Just testErr) -> Left (TestFailMsg, "Error, unit tests didn't all pass, fix them first. I encourage you to add more logging/printf for debugging if necessary, and to record your current step and planned future steps in the journal.txt . The last error was: " <> testErr)

clearJournal :: AppM ()
clearJournal = do
  cfg <- ask
  liftIO $ FS.clearFileOnDisk (FS.toFilePath cfg journalFileName)

makeUnitTests :: forall bs. (BS.BuildSystem bs) => Text -> ThingWithDependencies -> AppM ()
makeUnitTests background plannedFile = do
  let fileName = plannedFile.name
  cfg <- ask
  clearJournal
  resetCompileTestState
  modify' clearOpenFiles
  let dependencies = plannedFile.dependencies ++ [journalFileName, fileName]
  forM_ dependencies $ \x -> Tools.openFile x cfg
  let makeCtxt task =
        Context
          { contextBackground = background,
            contextTask = task,
            contextRest = []
          }
  let exampleUnitTests =
        UnitTests
          { unitTestFileName = "the_file_test.go",
            unitTests =
              [ UnitTest "testSomethingWorks" "Should test that ...",
                UnitTest "testsSomethingelseWorks" "Should test that ..."
              ]
          }
  let runner fileName' = runAiFunc @bs (makeCtxt (makeUnitTestsPrompt fileName')) allTools exampleUnitTests validateUnitTests (configTaskMaxFailures cfg)
  planRes <- memoise (configCacheDir cfg) "test_planner" fileName show runner
  let testFileName = unitTestFileName planRes
  Tools.openFile testFileName cfg
  let exampleUnitTestDone = UnitTestDone True
  let makeUnitTest UnitTest {..} = runAiFunc @bs (makeCtxt $ makeUnitTestPrompt fileName testFileName unitTestName unitTestSummary) allTools exampleUnitTestDone validateUnitTest (configTaskMaxFailures cfg)
  forM_ (unitTests planRes) $ \test ->
    -- TODO: add _
    memoise (configCacheDir cfg) ("test_creator_" <> testFileName) test unitTestName makeUnitTest

makeFile :: forall bs. (BS.BuildSystem bs) => Text -> ThingWithDependencies -> AppM ()
makeFile background pf = do
  cfg <- ask
  clearJournal
  resetCompileTestState
  modify' clearOpenFiles
  let dependencies = pf.dependencies ++ [journalFileName]
  forM_ dependencies $ \x -> Tools.openFile x cfg
  let makeCtxt fileName =
        Context
          { contextBackground = background,
            contextTask = makeSourcefilePrompt fileName pf.summary,
            contextRest = []
          }
  let exampleCreatedFiles =
        CreatedFiles
          { createdFiles =
              [ CreatedFile "someCode.go" "Code that ...",
                CreatedFile "someCode2.go" "Code that ..."
              ]
          }
  isBuildable <- BS.isBuildableFile @bs pf.name
  when isBuildable $ do
    let runner fileName = runAiFunc @bs (makeCtxt fileName) allTools exampleCreatedFiles validateCreatedFiles (configTaskMaxFailures cfg)
    createdFiles <- memoise (configCacheDir cfg) "file_creator" pf.name id runner
    forM_ createdFiles $ \x -> modify' $ updateFileDesc (createdFileName x) (createdFileSummary x)
    makeUnitTests @bs background pf

makeProject :: forall bs. (BS.BuildSystem bs) => AppM ()
makeProject = do
  cfg <- ask
  liftIO $ DIR.createDirectoryIfMissing True cfg.configBaseDir
  setupRes <- BS.setupProject @bs cfg
  when (isJust setupRes) $ throwError $ "Error setting up base project dir: " <> show setupRes
  let tools = [Tools.ToolOpenFile, Tools.ToolCloseFile, Tools.ToolPanic, Tools.ToolReturn]
      background = projectSummary (T.pack cfg.configBaseDir)
      archPrompt = makeArchitectureDesignPrompt
      archCtxt =
        Context
          { contextBackground = background,
            contextTask = archPrompt,
            contextRest = []
          }
      exampleArch = ThingWithDescription "The architecture of the project will be as follows: ..."
      archRunner () = runAiFunc @bs archCtxt tools exampleArch validateAlwaysPass (configTaskMaxFailures cfg)
  plannedArch <- memoise (configCacheDir cfg) "architecture" () (const "") archRunner

  let ctxt =
        Context
          { contextBackground = background <> "\n The architecture will be as follows: \n" <> plannedArch.description,
            contextTask = makeFilenamesPrompt,
            contextRest = []
          }
      examplePlannedFiles =
        ThingsWithDependencies
          { items =
              [ ThingWithDependencies "someFile.go" "This file contains functionality for..." [],
                ThingWithDependencies "someOtherFile.go" "This file contains functionality for something different ..." ["someFile.go"]
              ]
          }
      runner () = runAiFunc @bs ctxt tools examplePlannedFiles validateThingsWithDependencies (configTaskMaxFailures cfg)
  plannedFiles <- memoise (configCacheDir cfg) "file_planner" () (const "") runner
  forM_ plannedFiles (makeFile @bs ctxt.contextBackground)
  finalState <- get
  liftIO $ Logging.logInfo "Final config" (show cfg)
  liftIO $ Logging.logInfo "Final state" (show $ stateMetrics finalState)
  liftIO . putTextLn . show $ cfg
  liftIO . putTextLn . show $ stateMetrics finalState
