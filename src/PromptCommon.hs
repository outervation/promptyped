{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PromptCommon where

import BuildSystem as BS
import Control.Monad.Except
import Core
import Data.Aeson as AE
import Data.Graph
import Data.Map qualified as Map
import Data.Text qualified as T
import Engine qualified
import FileSystem qualified as FS
import Memoise (memoise)
import PromptTexts
import Relude
import System.Directory qualified as DIR
import Tools qualified

journalFileName :: Text
journalFileName = "journal.txt"

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

data FileProposedChanges = FileProposedChanges
  { fileName :: Text,
    proposedChanges :: [ThingWithDependencies]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON FileProposedChanges

instance FromJSON FileProposedChanges

data FilesProposedChanges = FilesProposedChanges
  { filesProposedChanges :: [FileProposedChanges]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON FilesProposedChanges

instance FromJSON FilesProposedChanges

topologicalSortThingsWithDependencies :: [ExistingFile] -> ThingsWithDependencies -> Either Text [ThingWithDependencies]
topologicalSortThingsWithDependencies existingFiles (ThingsWithDependencies files) = do
  let alreadyExists name = name `elem` map existingFileName existingFiles
  -- 1. Build a map from fileName -> ThingWithDependencies
  let fileMap :: Map Text ThingWithDependencies
      fileMap = Map.fromList [(f.name, f) | f <- files]

  -- 2. Check every dependency to ensure it exists in fileMap
  forM_ files $ \pf -> do
    forM_ pf.dependencies $ \dep ->
      unless (Map.member dep fileMap || alreadyExists dep)
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

data ModifiedFile = ModifiedFile
  { modifiedFileName :: Text,
    modificationSummary :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON ModifiedFile

instance FromJSON ModifiedFile

validateCreatedFiles :: CreatedFiles -> AppM (Either (MsgKind, Text) [CreatedFile])
validateCreatedFiles cf = do
  st <- get
  let files = createdFiles cf
  compilationAndTestsOkay <- checkCompileTestResults
  case compilationAndTestsOkay of
    Left err -> pure $ Left err
    Right () -> do
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

readOnlyTools :: [Tools.Tool]
readOnlyTools = [Tools.ToolOpenFile, Tools.ToolCloseFile, Tools.ToolPanic, Tools.ToolReturn]

data UnitTestDone = UnitTestDone
  { unitTestPassedSuccessfully :: Bool
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON UnitTestDone

instance FromJSON UnitTestDone

checkCompileTestResults :: AppM (Either (MsgKind, Text) ())
checkCompileTestResults = do
  st <- get
  let res = stateCompileTestRes st
  pure $ case (compileRes res, testRes res) of
    (Nothing, Nothing) -> Right ()
    (Just compileErr, _) -> Left (CompileFailMsg, "Error, compilation failed, fix it before returning. Note that if you see a 'missing import path' compilation error, it may be because you forgot a closing ')' for the go import list. If you see 'is not a package path' when trying to import a local file you created, remember you should include 'project_name/filename', NOT '/home/username/project_name/filename' or 'username/project_name/filename' The last error was: " <> compileErr)
    (Nothing, Just testErr) -> Left (TestFailMsg, "Error, unit tests didn't all pass (or failed to compile), fix them first. I encourage you to add more logging/printf for debugging if necessary, and to record your current step and planned future steps in the journal.txt . The last error was: " <> testErr)

validateUnitTest :: UnitTestDone -> AppM (Either (MsgKind, Text) ())
validateUnitTest t = case unitTestPassedSuccessfully t of
  False -> pure $ Left (OtherMsg, "Your return value of false indicates it didn't pass successfully")
  True -> checkCompileTestResults

clearJournal :: AppM ()
clearJournal = do
  cfg <- ask
  liftIO $ FS.clearFileOnDisk (FS.toFilePath cfg journalFileName)

makeUnitTestsInner :: forall bs. (BS.BuildSystem bs) => Text -> Text -> (Text -> Text) -> AppM ()
makeUnitTestsInner background fileName makeTestPrompt = do
  cfg <- ask
  let unitTestExampleFileName = T.replace fileName " .go" "_test.go"
  Tools.openFile fileName cfg
  let makeCtxt task =
        Context
          { contextBackground = background,
            contextTask = task,
            contextRest = []
          }
  let exampleUnitTests =
        UnitTests
          { unitTestFileName = unitTestExampleFileName,
            unitTests =
              [ UnitTest "testSomethingWorks" "Should test that ...",
                UnitTest "testsSomethingelseWorks" "Should test that ..."
              ]
          }
  let runner fileName' = Engine.runAiFunc @bs (makeCtxt $ makeTestPrompt fileName') allTools exampleUnitTests validateUnitTests (configTaskMaxFailures cfg)
  planRes <- memoise (configCacheDir cfg) "test_planner" fileName show runner
  let testFileName = unitTestFileName planRes
  Tools.openFile testFileName cfg
  let exampleUnitTestDone = UnitTestDone True
  let makeUnitTest UnitTest {..} = Engine.runAiFunc @bs (makeCtxt $ makeUnitTestPrompt fileName testFileName unitTestName unitTestSummary) allTools exampleUnitTestDone validateUnitTest (configTaskMaxFailures cfg)
  forM_ (unitTests planRes) $ \test ->
    memoise (configCacheDir cfg) ("test_creator_" <> testFileName) test unitTestName makeUnitTest

makeUnitTests :: forall bs. (BS.BuildSystem bs) => Text -> ThingWithDependencies -> AppM ()
makeUnitTests background plannedFile = do
  let fileName = plannedFile.name
  cfg <- ask
  clearJournal
  modify' clearOpenFiles
  let dependencies = [fileName, journalFileName] ++ plannedFile.dependencies
  forM_ dependencies $ \x -> Tools.openFile x cfg
  makeUnitTestsInner @bs background fileName makeUnitTestsPrompt

makeFile :: forall bs. (BS.BuildSystem bs) => Text -> ThingWithDependencies -> AppM ()
makeFile background pf = do
  cfg <- ask
  resetCompileTestState
  modify' clearOpenFiles
  let dependencies = [pf.name, journalFileName] ++ pf.dependencies
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
    let runner fileName = Engine.runAiFunc @bs (makeCtxt fileName) allTools exampleCreatedFiles validateCreatedFiles (configTaskMaxFailures cfg)
    createdFiles <- memoise (configCacheDir cfg) "file_creator" pf.name id runner
    forM_ createdFiles $ \x -> modify' $ updateFileDesc (createdFileName x) (createdFileSummary x)
    makeUnitTests @bs background pf

makeRefactorFileTask :: forall bs. (BS.BuildSystem bs) => Text -> [ExistingFile] -> Text -> [ThingWithDependencies] -> AppM ()
makeRefactorFileTask background initialDeps fileName desiredChanges = do
  cfg <- ask
  modify' clearOpenFiles
  -- Note: file opened first will appear last (most recent)
  let dependencies = [fileName, journalFileName] ++ map (\x -> x.existingFileName) initialDeps
  forM_ dependencies $ \x -> Tools.openFile x cfg
  let makeChange description = do
        let ctxt =
              Context
                { contextBackground = background,
                  contextTask = "Your task is to refactor the file " <> fileName <> " to make the change: " <> show description,
                  contextRest = []
                }
            exampleChange = ModifiedFile "someFile.go" "Update the file to add ... so that it ..."
        Engine.runAiFunc @bs ctxt allTools exampleChange validateAlwaysPass (configTaskMaxFailures cfg)
  forM_ desiredChanges $ \x -> do
    modification <- memoise (configCacheDir cfg) ("file_modifier_" <> fileName) x (\desc -> desc.name) makeChange
    let modificationText = "Intended modification: " <> x.summary <> ", with model describing what it did as " <> show modification <> "."
    makeUnitTestsInner @bs background fileName $ makeUnitTestsForSpecificChangePrompt modificationText

makeRefactorFilesProject :: forall bs. (BS.BuildSystem bs) => AppM ()
makeRefactorFilesProject = do
  st <- get
  cfg <- ask
  ignoredDirs <- BS.getIgnoredDirs @bs
  let docFileName = "binanceApiDetails_CoinMFutures.txt"
      docFilePath = FS.toFilePath cfg docFileName
  docAlreadyExists <- liftIO $ FS.fileExistsOnDisk docFilePath
  unless docAlreadyExists $ do
    createDocRes <- liftIO $ FS.appendToFile docFilePath binanceFuturesApiDoc
    when (isLeft createDocRes) $ throwError $ "Error creating docs: " <> show createDocRes
  let setupOpenFiles fileName = do
        modify' clearOpenFiles
        forM_ [fileName, journalFileName, docFileName] $ \x -> Tools.openFile x cfg
  existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs cfg.configBaseDir
  modify' (updateExistingFiles existingFileNames)
  sourceFileNames <- filterM (BS.isBuildableFile @bs) $ map existingFileName st.stateFiles
  let task =
        "YOUR OBJECTIVE is to refactor the project to add support for Binance CoinM futures market data (it currently only supports Binance spot), as described in binanceApiDetails_CoinMFutures.txt."
          <> "Note that the datatypes may be slightly different than for Binance spot; when this is the case you should create different structs for each, and store them in different parquet tables to the existing types."
          <> "The data should be saved to filenames containing the kind (spot or future), date and instrument, not just the date and instrument."
          <> "The config should be kind,instrument pairs, not just instrument, and depending on kind the code will properly pick and connect to Binance Spot or Futures."
          <> "You need to support aggregate trade streams, individual symbol book ticker streams, partial book depth streams, diff book depth streams, and mark price streams. Remember to implement logic so the data can be used for managing a local orderbook correctly, as already done for Binance Spot; how to do this is described in the doc."
      objectiveShortName = "Add support for Binance CoinM futures"
      refactorBackground = makeRefactorBackgroundPrompt task
      background = projectSummary (T.pack cfg.configBaseDir) <> "\n" <> refactorBackground
      exampleThingsWithDependencies =
        [ ThingWithDependencies "addNewClassX" "Class X, which does ..., must be added to support ..." [],
          ThingWithDependencies "addNewFuncY" "Function Y, which does ..., must be added to support ..." ["addNewClassX"]
        ]
      exampleProposedChanges = ThingsWithDependencies exampleThingsWithDependencies
      getChangesTask fileName = do
        let ctxt =
              Context
                { contextBackground = background,
                  contextTask =
                    "Please return a list of tasks that must be done to refactor "
                      <> fileName
                      <> " to achieve the above objective ("
                      <> objectiveShortName
                      <> ")."
                      <> "Each task should list other task dependencies if any, and there should be no circular dependencies."
                      <> "If there's something relevant for later that you can't encode well in the return value, please AppendFile=<[{\"fileName\": \"journal.txt\", \"rawTextName\": \"journalUpdateTextBoxName\"}]> it to the journal.",
                  contextRest = []
                }
        setupOpenFiles fileName
        Engine.runAiFunc @bs ctxt (Tools.ToolAppendFile : readOnlyTools) exampleProposedChanges validateThingsWithDependencies (configTaskMaxFailures cfg)
  plannedTasks <- forM_ sourceFileNames $ \fileName -> do
    fileTasks <- memoise (configCacheDir cfg) "file_dependencies" fileName id getChangesTask
    return $ FileProposedChanges fileName fileTasks
  let combineCtxt =
        Context
          { contextBackground = background,
            contextTask =
              "You've previously just produced a list per file of tasks that must be done to refactor each file to achieve the above objective ("
                <> objectiveShortName
                <> ")."
                <> "Now please edit/update these where necessary to account for you now having vision of all tasks (previously you created each file's tasks independently), to e.g. remove duplication and make the overall plan coherent, and return the updated list of tasks."
                <> "If there's something relevant for later that you can't encode well in the return value, please AppendFile=<[{\"fileName\": \"journal.txt\", \"rawTextName\": \"journalUpdateTextBoxName\"}]> it to the journal."
                <> "The previously created tasks are: \n"
                <> Tools.toJ plannedTasks,
            contextRest = []
          }
      combineExample =
        FilesProposedChanges
          [ FileProposedChanges "someFile.go" exampleThingsWithDependencies,
            FileProposedChanges "someOtherFile.go" exampleThingsWithDependencies
          ]
      refineChangesTask () = Engine.runAiFunc @bs combineCtxt (Tools.ToolAppendFile : readOnlyTools) combineExample validateAlwaysPass (configTaskMaxFailures cfg)
  modify' clearOpenFiles
  Tools.openFile docFileName cfg
  Tools.openFile journalFileName cfg
  plannedTasksRefined <- memoise (configCacheDir cfg) "all_file_dependencies" () (const "") refineChangesTask

  let extraFilesCtxt =
        Context
          { contextBackground = background,
            contextTask =
              "You've previously just produced a list per file of tasks that must be done to refactor each file to achieve the above objective ("
                <> objectiveShortName
                <> ")."
                <> "Now please think carefully about if there's any new files that will need to be created, due to some changes not fitting well into existing files, and return a list of such files along with detailed descriptions of each, and their dependencies if any."
                <> "The previously created tasks are: \n"
                <> Tools.toJ plannedTasks,
            contextRest = []
          }
      exampleExtraFiles =
        ThingsWithDependencies
          { items =
              [ ThingWithDependencies "someFile.go" "This file contains functionality for..." [],
                ThingWithDependencies "someOtherFile.go" "This file contains functionality for something different ..." ["someFile.go"]
              ]
          }
      getExtraFilesTask () = Engine.runAiFunc @bs extraFilesCtxt readOnlyTools exampleExtraFiles validateThingsWithDependencies (configTaskMaxFailures cfg)
  extraFilesNeeded <- memoise (configCacheDir cfg) "all_extra_files" () (const "") getExtraFilesTask
  let makeFileBackground = background <> "\n You are currently working on adding some extra files that are necessary as part of the refactoring."
  forM_ extraFilesNeeded (makeFile @bs makeFileBackground)
  let docDeps = [ExistingFile docFileName ""]
  forM_ plannedTasksRefined.filesProposedChanges $ \x -> makeRefactorFileTask @bs background docDeps x.fileName x.proposedChanges

makeCreateFilesProject :: forall bs. (BS.BuildSystem bs) => AppM ()
makeCreateFilesProject = do
  cfg <- ask
  liftIO $ DIR.createDirectoryIfMissing True cfg.configBaseDir
  setupRes <- BS.setupProject @bs cfg
  when (isJust setupRes) $ throwError $ "Error setting up base project dir: " <> show setupRes
  ignoredDirs <- BS.getIgnoredDirs @bs
  existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs cfg.configBaseDir
  modify' (updateExistingFiles existingFileNames)
  let background = projectSummary (T.pack cfg.configBaseDir)
      archPrompt = makeArchitectureDesignPrompt
      archCtxt =
        Context
          { contextBackground = background,
            contextTask = archPrompt,
            contextRest = []
          }
      exampleArch = ThingWithDescription "The architecture of the project will be as follows: ..."
      archRunner () = Engine.runAiFunc @bs archCtxt readOnlyTools exampleArch validateAlwaysPass (configTaskMaxFailures cfg)
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
      runner () = Engine.runAiFunc @bs ctxt readOnlyTools examplePlannedFiles validateThingsWithDependencies (configTaskMaxFailures cfg)
  plannedFiles <- memoise (configCacheDir cfg) "file_planner" () (const "") runner
  forM_ plannedFiles (makeFile @bs ctxt.contextBackground)
