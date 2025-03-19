{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PromptCommon where

import BuildSystem as BS
import Control.Monad.Except
import Control.Monad.Loops (untilM_)
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

data ProjectTexts = ProjectTexts
  { projectSummaryText :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON ProjectTexts

instance FromJSON ProjectTexts

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

compilationAndTestsPass :: AppM Bool
compilationAndTestsPass = do
  st <- get
  let res = stateCompileTestRes st
      cErr = compileRes res
      tErr = testRes res
  return $ case (cErr, tErr) of
    (Nothing, Nothing) -> True
    _ -> False

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

validatePropertyOfThingsWithDependencies :: ThingsWithDependencies -> (ThingWithDependencies -> Maybe Text) -> Either (MsgKind, Text) ThingsWithDependencies
validatePropertyOfThingsWithDependencies (ThingsWithDependencies things) validator =
  case catMaybes (map validator things) of
    [] -> Right $ ThingsWithDependencies things
    errors -> Left (OtherMsg, T.intercalate ", " errors)

validatePathNotNested :: ThingWithDependencies -> Maybe Text
validatePathNotNested thing =
  if T.isInfixOf "/" thing.name
    then Just $ "Error; nested paths are not allowed, but " <> thing.name <> " is a nested path."
    else Nothing

validateThingsWithDependencies :: ThingsWithDependencies -> AppM (Either (MsgKind, Text) [ThingWithDependencies])
validateThingsWithDependencies pf = do
  st <- get
  return $ either (\x -> Left (OtherMsg, x)) Right $ topologicalSortThingsWithDependencies (stateFiles st) pf

validateFileNamesNoNestedPaths :: ThingsWithDependencies -> AppM (Either (MsgKind, Text) [ThingWithDependencies])
validateFileNamesNoNestedPaths things = do
  case validatePropertyOfThingsWithDependencies things validatePathNotNested of
    Left err -> pure $ Left err
    Right things' -> validateThingsWithDependencies things'

data ThingWithDescription = ThingWithDescription
  { description :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON ThingWithDescription

instance FromJSON ThingWithDescription

validateAlwaysPass :: a -> AppM (Either (MsgKind, Text) a)
validateAlwaysPass x = pure $ Right x

validateAlwaysPassIfCompileTestsFine :: a -> AppM (Either (MsgKind, Text) a)
validateAlwaysPassIfCompileTestsFine x = do
  res <- checkCompileTestResults
  pure $ bimap id (const x) res

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
allTools = [Tools.ToolOpenFile, Tools.ToolCloseFile, Tools.ToolAppendFile, Tools.ToolInsertInFile, Tools.ToolEditFileByMatch, Tools.ToolPanic, Tools.ToolReturn]

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
    (Nothing, Just testErr) -> Left (TestFailMsg, "Error, unit tests didn't all pass (or failed to compile), fix them first. I encourage you to add more logging/printf for debugging if necessary, and to record your current step and planned future steps in the journal.txt . If you see 'is not a package path' when trying to import a local file you created into a test, remember you should include 'project_name/filename', NOT '/home/username/project_name/filename' or 'username/project_name/filename, and put everything in package main. The last error was: " <> testErr)

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
  let makeCtxt task = makeBaseContext background task
      exampleUnitTests =
        UnitTests
          { unitTestFileName = unitTestExampleFileName,
            unitTests =
              [ UnitTest "testSomethingWorks" "Should test that ...",
                UnitTest "testsSomethingelseWorks" "Should test that ..."
              ]
          }
      runner fileName' = Engine.runAiFunc @bs (makeCtxt $ makeTestPrompt fileName') MediumIntelligenceRequired allTools exampleUnitTests validateUnitTests (configTaskMaxFailures cfg)
  planRes <- memoise (configCacheDir cfg) "test_planner" fileName id runner
  let testFileName = unitTestFileName planRes
  Tools.openFile testFileName cfg
  let exampleUnitTestDone = UnitTestDone True
  let makeUnitTest UnitTest {..} = Engine.runAiFunc @bs (makeCtxt $ makeUnitTestPrompt fileName testFileName unitTestName unitTestSummary) MediumIntelligenceRequired allTools exampleUnitTestDone validateUnitTest (configTaskMaxFailures cfg)
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

makeFile :: forall bs. (BS.BuildSystem bs) => Text -> [Text] -> ThingWithDependencies -> AppM ()
makeFile background extraFiles pf = do
  cfg <- ask
  resetCompileTestState
  modify' clearOpenFiles
  let dependencies = [pf.name, journalFileName] ++ pf.dependencies ++ extraFiles
  forM_ dependencies $ \x -> Tools.openFile x cfg
  let makeCtxt fileName = makeBaseContext background $ makeSourcefilePrompt fileName pf.summary
      exampleCreatedFiles =
        CreatedFiles
          { createdFiles =
              [ CreatedFile "someCode.go" "Code that ...",
                CreatedFile "someCode2.go" "Code that ..."
              ]
          }
  isBuildable <- BS.isBuildableFile @bs pf.name
  when isBuildable $ do
    let runner fileName = Engine.runAiFunc @bs (makeCtxt fileName) MediumIntelligenceRequired allTools exampleCreatedFiles validateCreatedFiles (configTaskMaxFailures cfg)
    createdFiles <- memoise (configCacheDir cfg) "file_creator" pf.name id runner
    forM_ createdFiles $ \x -> modify' $ updateFileDesc (createdFileName x) (createdFileSummary x)
    makeUnitTests @bs background pf

data AutoRefactorUnitTests = DoAutoRefactorUnitTests | DontAutoRefactorUnitTests
  deriving (Eq, Ord, Show)

makeRefactorFileTask :: forall bs. (BS.BuildSystem bs) => Text -> [ExistingFile] -> Text -> [ThingWithDependencies] -> AutoRefactorUnitTests -> AppM ()
makeRefactorFileTask background initialDeps fileName desiredChanges refactorUnitTests = do
  cfg <- ask
  modify' clearOpenFiles
  -- Note: file opened first will appear last (most recent)
  let dependencies = [fileName, journalFileName] ++ map (\x -> x.existingFileName) initialDeps
  forM_ dependencies $ \x -> Tools.openFile x cfg
  let makeChange description = do
        let ctxt = makeBaseContext background $ "Your task is to refactor the file " <> fileName <> " to make the change: " <> show description
            exampleChange = ModifiedFile "someFile.go" "Update the file to add ... so that it ..."
        Engine.runAiFunc @bs ctxt MediumIntelligenceRequired allTools exampleChange validateAlwaysPassIfCompileTestsFine (configTaskMaxFailures cfg)
  modifications <- forM desiredChanges $ \x -> do
    modification <- memoise (configCacheDir cfg) ("file_modifier_" <> fileName) x (\desc -> desc.name) makeChange
    putTextLn $ "Done task: " <> show x
    return $ "Intended modification: " <> x.summary <> ", with model describing what it did as " <> show modification <> "."
  let modificationsTxt = "The model made the following changes: \n" <> T.unlines modifications
  when (refactorUnitTests == DoAutoRefactorUnitTests)
    $ makeUnitTestsInner @bs background fileName
    $ makeUnitTestsForSpecificChangePrompt modificationsTxt

data BigRefactorConfig = BigRefactorConfig
  { bigRefactorInitialOpenFiles :: [Text],
    bigRefactorOverallTask :: Text,
    bigRefactorOverallTaskShortName :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON BigRefactorConfig

instance FromJSON BigRefactorConfig

makeRefactorFilesProject :: forall bs. (BS.BuildSystem bs) => ProjectTexts -> BigRefactorConfig -> AppM ()
makeRefactorFilesProject projectTexts refactorCfg = do
  cfg <- ask
  ignoredDirs <- BS.getIgnoredDirs @bs
  let setupOpenFiles fileName = do
        modify' clearOpenFiles
        forM_ ([fileName, journalFileName] ++ refactorCfg.bigRefactorInitialOpenFiles) $ \x -> Tools.openFile x cfg
  existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs cfg.configBaseDir
  modify' (updateExistingFiles existingFileNames)
  st <- get
  sourceFileNames <- filterM (BS.isBuildableFile @bs) $ map existingFileName st.stateFiles
  let fixErrs = fixFailedTestsAndCompilation @bs refactorCfg.bigRefactorOverallTask refactorCfg.bigRefactorInitialOpenFiles
  fixErrs
  untilM_ fixErrs compilationAndTestsPass
  let task = refactorCfg.bigRefactorOverallTask
      objectiveShortName = refactorCfg.bigRefactorOverallTaskShortName
      refactorBackground = makeRefactorBackgroundPrompt task
      background = projectTexts.projectSummaryText <> "\n" <> refactorBackground
      exampleThingsWithDependencies =
        [ ThingWithDependencies "addNewClassX" "Class X, which does ..., must be added to support ..." [],
          ThingWithDependencies "addNewFuncY" "Function Y, which does ..., must be added to support ..." ["addNewClassX"]
        ]
      exampleProposedChanges = ThingsWithDependencies exampleThingsWithDependencies
      getChangesTask fileName = do
        let ctxt =
              makeBaseContext background
                $ "Please return a list of tasks that must be done (if any; there may be none) to refactor "
                <> fileName
                <> " to achieve the above objective ("
                <> objectiveShortName
                <> ")."
                <> "Each task should list other task dependencies if any, and there should be no circular dependencies."
                <> "If there's something relevant for later that you can't encode well in the return value, please AppendFile=<[{\"fileName\": \"journal.txt\", \"rawTextName\": \"journalUpdateTextBoxName\"}]> it to the journal."
        setupOpenFiles fileName
        Engine.runAiFunc @bs ctxt MediumIntelligenceRequired (Tools.ToolAppendFile : readOnlyTools) exampleProposedChanges validateThingsWithDependencies (configTaskMaxFailures cfg)
  plannedTasks <- forM_ sourceFileNames $ \fileName -> do
    fileTasks <- memoise (configCacheDir cfg) "file_dependencies" fileName id getChangesTask
    return $ FileProposedChanges fileName fileTasks
  let combineCtxt =
        makeBaseContext background
          $ "You've previously just produced a list per file of tasks that must be done to refactor each file to achieve the above objective ("
          <> objectiveShortName
          <> ")."
          <> "Now please edit/update these where necessary to account for you now having vision of all tasks (previously you created each file's tasks independently), to e.g. remove duplication and make the overall plan coherent, and return the updated list of tasks."
          <> "If there's something relevant for later that you can't encode well in the return value, please AppendFile=<[{\"fileName\": \"journal.txt\", \"rawTextName\": \"journalUpdateTextBoxName\"}]> it to the journal."
          <> "The previously created tasks are: \n"
          <> Tools.toJ plannedTasks
      combineExample =
        FilesProposedChanges
          [ FileProposedChanges "someFile.go" exampleThingsWithDependencies,
            FileProposedChanges "someOtherFile.go" exampleThingsWithDependencies
          ]
      refineChangesTask () = Engine.runAiFunc @bs combineCtxt HighIntelligenceRequired (Tools.ToolAppendFile : readOnlyTools) combineExample validateAlwaysPass (configTaskMaxFailures cfg)
  modify' clearOpenFiles
  forM_ (journalFileName : refactorCfg.bigRefactorInitialOpenFiles) $ \x -> Tools.openFile x cfg
  plannedTasksRefined <- memoise (configCacheDir cfg) "all_file_dependencies" () (const "") refineChangesTask

  let extraFilesCtxt =
        makeBaseContext background
          $ "You've previously just produced a list per file of tasks that must be done to refactor each file to achieve the above objective ("
          <> objectiveShortName
          <> ")."
          <> "Now please think carefully about if there's any new files that will need to be created, due to some changes not fitting well into existing files, and return a list of such files along with detailed descriptions of each, and their dependencies if any."
          <> "The previously created tasks are: \n"
          <> Tools.toJ plannedTasks
      exampleExtraFiles =
        ThingsWithDependencies
          { items =
              [ ThingWithDependencies "someFile.go" "This file contains functionality for..." [],
                ThingWithDependencies "someOtherFile.go" "This file contains functionality for something different ..." ["someFile.go"]
              ]
          }
      getExtraFilesTask () = Engine.runAiFunc @bs extraFilesCtxt HighIntelligenceRequired readOnlyTools exampleExtraFiles validateFileNamesNoNestedPaths (configTaskMaxFailures cfg)
  extraFilesNeeded <- memoise (configCacheDir cfg) "all_extra_files" () (const "") getExtraFilesTask
  let makeFileBackground = background <> "\n You are currently working on adding some extra files that are necessary as part of the refactoring."
  forM_ extraFilesNeeded (makeFile @bs makeFileBackground refactorCfg.bigRefactorInitialOpenFiles)
  let docDeps = map (\x -> ExistingFile x "") refactorCfg.bigRefactorInitialOpenFiles
  forM_ plannedTasksRefined.filesProposedChanges $ \x -> makeRefactorFileTask @bs background docDeps x.fileName x.proposedChanges DoAutoRefactorUnitTests

makeCreateFilesProject :: forall bs. (BS.BuildSystem bs) => ProjectTexts -> ProjectConfig -> AppM ()
makeCreateFilesProject projectTexts projectCfg = do
  cfg <- ask
  liftIO $ DIR.createDirectoryIfMissing True cfg.configBaseDir
  setupRes <- BS.setupProject @bs cfg projectCfg
  when (isJust setupRes) $ throwError $ "Error setting up base project dir: " <> show setupRes
  ignoredDirs <- BS.getIgnoredDirs @bs
  existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs cfg.configBaseDir
  modify' (updateExistingFiles existingFileNames)
  let background = projectTexts.projectSummaryText
      archPrompt = makeArchitectureDesignPrompt
      archCtxt = makeBaseContext background archPrompt
      exampleArch = ThingWithDescription "The architecture of the project will be as follows: ..."
      archRunner () = Engine.runAiFunc @bs archCtxt HighIntelligenceRequired readOnlyTools exampleArch validateAlwaysPass (configTaskMaxFailures cfg)
  plannedArch <- memoise (configCacheDir cfg) "architecture" () (const "") archRunner

  let ctxt = makeBaseContext (background <> "\n The architecture will be as follows: \n" <> plannedArch.description) makeFilenamesPrompt
      examplePlannedFiles =
        ThingsWithDependencies
          { items =
              [ ThingWithDependencies "someFile.go" "This file contains functionality for..." [],
                ThingWithDependencies "someOtherFile.go" "This file contains functionality for something different ..." ["someFile.go"]
              ]
          }
      runner () = Engine.runAiFunc @bs ctxt HighIntelligenceRequired readOnlyTools examplePlannedFiles validateFileNamesNoNestedPaths (configTaskMaxFailures cfg)
  plannedFiles <- memoise (configCacheDir cfg) "file_planner" () (const "") runner
  forM_ plannedFiles (makeFile @bs ctxt.contextBackground [])

data TargetedRefactorConfigItem = TargetedRefactorConfigItem
  { refactorFile :: Text,
    refactorTask :: Text,
    refactorFileDependencies :: [Text],
    refactorUpdateTests :: Bool
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON TargetedRefactorConfigItem

instance FromJSON TargetedRefactorConfigItem

data TargetedRefactorConfig = TargetedRefactorConfig
  { refactorSummary :: Text,
    refactorFileTasks :: [TargetedRefactorConfigItem]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON TargetedRefactorConfig

instance FromJSON TargetedRefactorConfig

makeTargetedRefactorProject :: forall bs. (BS.BuildSystem bs) => ProjectTexts -> TargetedRefactorConfig -> AppM ()
makeTargetedRefactorProject projectTexts refactorCfg = do
  cfg <- ask
  ignoredDirs <- BS.getIgnoredDirs @bs
  existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs cfg.configBaseDir
  modify' (updateExistingFiles existingFileNames)
  let setupOpenFiles fileNames = do
        modify' clearOpenFiles
        forM_ (fileNames ++ [journalFileName]) $ \x -> Tools.openFile x cfg
  let summary = refactorCfg.refactorSummary
      doRefactor :: TargetedRefactorConfigItem -> AppM ()
      doRefactor rCfg = do
        origSt <- get
        unless (fileExists rCfg.refactorFile origSt) $ throwError $ "Trying to refactor file that doesn't exist: " <> rCfg.refactorFile
        let background = projectTexts.projectSummaryText <> "\n" <> summary
            exampleTasks =
              ThingsWithDependencies
                $ [ ThingWithDependencies "addNewClassX" "Class X, which does ..., must be added to support ..." [],
                    ThingWithDependencies "addNewFuncY" "Function Y, which does ..., must be added to support ..." ["addNewClassX"]
                  ]
            relFiles = rCfg.refactorFile : rCfg.refactorFileDependencies
            autoRefactorUnitTests = if rCfg.refactorUpdateTests then DoAutoRefactorUnitTests else DontAutoRefactorUnitTests
            mkCtxt fileName =
              makeBaseContext background
                $ "Please return a list of tasks that must be done to refactor "
                <> fileName
                <> " to achieve the objective: "
                <> rCfg.refactorTask
                <> "."
                <> "Each task should list other task dependencies if any, and there should be no circular dependencies."
                <> "If there's something relevant for later that you can't encode well in the return value, please AppendFile=<[{\"fileName\": \"journal.txt\", \"rawTextName\": \"journalUpdateTextBoxName\"}]> it to the journal."
            taskBackground = background <> "\nYour task for this file is: " <> rCfg.refactorTask
            getChangesTask fileName = Engine.runAiFunc @bs (mkCtxt fileName) HighIntelligenceRequired (Tools.ToolAppendFile : readOnlyTools) exampleTasks validateThingsWithDependencies (configTaskMaxFailures cfg)
        setupOpenFiles relFiles
        plannedTasks <- memoise (configCacheDir cfg) "file_tasks" rCfg.refactorFile id getChangesTask
        st <- get
        case getExistingFiles rCfg.refactorFileDependencies st of
          Left err -> throwError $ "Dependency listed for " <> rCfg.refactorFile <> " does not exist: " <> err
          Right deps -> makeRefactorFileTask @bs taskBackground deps rCfg.refactorFile plannedTasks autoRefactorUnitTests
  forM_ refactorCfg.refactorFileTasks doRefactor

data FileAnalysisResult = FileAnalysisResult
  { waysItDoesntMeetSpec :: Text,
    overallContentSummary :: Text
  }
  deriving (Generic, Eq, Ord, Show)

renderFileAnalysisResult :: FileAnalysisResult -> Text
renderFileAnalysisResult (FileAnalysisResult spec overall) = "FileAnalysisResult{\n waysItDoesntMeetSpec: " <> spec <> "\n,\n overallContentSummary: " <> overall <> "\n}"

instance ToJSON FileAnalysisResult

instance FromJSON FileAnalysisResult

makeFileAnalysisProject :: forall bs. (BS.BuildSystem bs) => ProjectTexts -> AppM ()
makeFileAnalysisProject projectTexts = do
  cfg <- ask
  ignoredDirs <- BS.getIgnoredDirs @bs
  existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs cfg.configBaseDir
  modify' (updateExistingFiles existingFileNames)
  st <- get
  sourceFileNames <- filterM (BS.isBuildableFile @bs) $ map existingFileName st.stateFiles
  let background = projectTexts.projectSummaryText
      setupOpenFile fileName = do
        modify' clearOpenFiles
        Tools.openFile fileName cfg
      getSummary :: Text -> AppM (Text, FileAnalysisResult)
      getSummary fileName = do
        let mkCtxt name =
              makeBaseContext background
                $ "Please check if "
                <> name
                <> " matches the specification, and return any ways it fails to satisfy it. Please also return detailed notes on its behaviour, for reference when checking other files."
            exampleRes = FileAnalysisResult (fileName <> "doesn't meet the spec completely because it's supposed to ..., but it doesn't, and ...") "The file fulfills the following spec-relevant behaviours:"
            getChangesTask name = Engine.runAiFunc @bs (mkCtxt name) MediumIntelligenceRequired readOnlyTools exampleRes validateAlwaysPass (configTaskMaxFailures cfg)
        setupOpenFile fileName
        fileRes <- memoise (configCacheDir cfg) "file_analysis" fileName id getChangesTask
        return (fileName, fileRes)
  summaries <- forM sourceFileNames getSummary
  let summariesCat = T.unlines $ map (\(name, res) -> name <> ":\n" <> renderFileAnalysisResult res) summaries
      combinedSummaryCtxt =
        makeBaseContext background
          $ "You previously analysed files in the project to identify any way they failed to match the spec. Now, based on the result of your analysis (which was per-file), can you see any other ways in which overall the project fails to match the spec? The analysis was:\n"
          <> summariesCat
      exampleCombinedRes = ThingWithDescription "Overall the project doesn't have any files that fulfil the ... requirement completely because ..."
      getCombinedSummary () = Engine.runAiFunc @bs combinedSummaryCtxt HighIntelligenceRequired readOnlyTools exampleCombinedRes validateAlwaysPass (configTaskMaxFailures cfg)
  modify' clearOpenFiles
  combinedSummary <- memoise (configCacheDir cfg) "file_analysis_combined" () show getCombinedSummary
  let summaryFileName = "specComplianceSummary.txt"
      summaryFilePath = FS.toFilePath cfg summaryFileName
      finalResult = summariesCat <> "\n" <> combinedSummary.description
  liftIO $ FS.clearFileOnDisk summaryFilePath
  writeRes <- liftIO $ FS.appendToFile summaryFilePath finalResult
  case writeRes of
    Left err -> throwError $ "Failed to write to " <> T.pack summaryFilePath <> " due to " <> err <> " with text: \n" <> finalResult
    Right () -> putTextLn $ "Wrote result to " <> T.pack summaryFilePath

data SpecSegmentPlan = SpecSegmentPlan
  { segmentFileName :: Text,
    segmentTitle :: Text,
    startLineNum :: Int,
    endLineNum :: Int
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON SpecSegmentPlan

instance FromJSON SpecSegmentPlan

data SpecSegmentPlans = SpecSegmentPlans
  { segmentPlans :: [SpecSegmentPlan]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON SpecSegmentPlans

instance FromJSON SpecSegmentPlans

validateSpecSegmentPlans ::
  -- | docLength (total number of lines in the spec)
  Int ->
  -- | the proposed chunking plan
  SpecSegmentPlans ->
  AppM (Either (MsgKind, Text) SpecSegmentPlans)
validateSpecSegmentPlans docLength ssp@(SpecSegmentPlans segments) = do
  let errors = concatMap checkSegment segments <> checkNoOverlaps (sortOn (.startLineNum) segments)

  if null errors
    then pure $ Right ssp
    else pure $ Left (OtherMsg, T.unlines errors)
  where
    -- \| Validate an individual segment
    checkSegment :: SpecSegmentPlan -> [Text]
    checkSegment SpecSegmentPlan {..} =
      let errs =
            [ "startLineNum must be >= 1, but got: " <> show startLineNum
            | startLineNum < 1
            ]
              <> [ "endLineNum must be >= 1, but got: " <> show endLineNum
                 | endLineNum < 1
                 ]
              <> [ "endLineNum (" <> show endLineNum <> ") cannot exceed docLength (" <> show docLength <> ")"
                 | endLineNum > docLength
                 ]
              <> [ "startLineNum (" <> show startLineNum <> ") cannot exceed endLineNum (" <> show endLineNum <> ")"
                 | startLineNum > endLineNum
                 ]
       in errs

    -- \| Ensure no overlapping segments when sorted by startLineNum.
    --   Because ranges are inclusive, we require that:
    --      next.startLineNum > current.endLineNum
    checkNoOverlaps :: [SpecSegmentPlan] -> [Text]
    checkNoOverlaps [] = []
    checkNoOverlaps [_] = []
    checkNoOverlaps (x : y : rest) =
      let e =
            if y.startLineNum <= x.endLineNum
              then
                [ "Overlapping segments detected: ("
                    <> x.segmentFileName
                    <> " has range "
                    <> show (x.startLineNum, x.endLineNum)
                    <> ") overlaps with ("
                    <> y.segmentFileName
                    <> " has range "
                    <> show (y.startLineNum, y.endLineNum)
                    <> ")"
                ]
              else []
       in e <> checkNoOverlaps (y : rest)

--------------------------------------------------------------------------------

makeCreateBasedOnSpecProject ::
  forall bs.
  (BS.BuildSystem bs) =>
  ProjectTexts ->
  -- | path or filename of the spec
  Text ->
  ProjectConfig ->
  AppM ()
makeCreateBasedOnSpecProject projectTexts specFileName projectCfg = do
  cfg <- ask
  liftIO $ DIR.createDirectoryIfMissing True (configBaseDir cfg)
  setupRes <- BS.setupProject @bs cfg projectCfg
  when (isJust setupRes)
    $ throwError
    $ "Error setting up base project dir: "
    <> show setupRes

  -- 1) Read entire spec
  let specPathOnDisk = FS.toFilePath cfg specFileName
  specExists <- liftIO $ FS.fileExistsOnDisk specPathOnDisk
  unless specExists
    $ throwError
    $ "Spec file does not exist: "
    <> T.pack specPathOnDisk

  allSpecText <- liftIO $ FS.readFileToText specPathOnDisk
  let allSpecLines = T.lines allSpecText

  -- 2) Prompt for chunking plan
  let lineCount = length allSpecLines
      chunkCtxt =
        makeBaseContext projectTexts.projectSummaryText
          $ "We have a specification in "
          <> specFileName
          <> " from lines 1.."
          <> show lineCount
          <> ". Please split it into multiple doc files, each covering a coherent subset. This allows the LLM to only load the relevant parts of the spec into context while working on each section.\n"
          <> "For each doc file, return:\n"
          <> "- segmentFileName\n"
          <> "- segmentTitle\n"
          <> "- startLineNum\n"
          <> "- endLineNum\n\n"
          <> "Return JSON describing the chunking plan for the spec."
      exampleSegments =
        SpecSegmentPlans
          { segmentPlans =
              [ SpecSegmentPlan "http_spec_part1.txt" "HTTP Request-Line and Headers" 1 100,
                SpecSegmentPlan "http_spec_part2.txt" "HTTP Response Formats" 101 200
              ]
          }

      -- If you want to allow journaling while chunking the spec,
      -- you can add `Tools.ToolAppendFile` to readOnlyTools:
      chunkingTools = Tools.ToolAppendFile : readOnlyTools

      getSegmentPlans () =
        Engine.runAiFunc @bs
          chunkCtxt
          HighIntelligenceRequired
          chunkingTools
          exampleSegments
          (validateSpecSegmentPlans lineCount)
          (configTaskMaxFailures cfg)

  segmentPlansResult <- memoise (configCacheDir cfg) "split_spec_into_docs" () (const "") getSegmentPlans

  -- 3) Write out doc files
  forM_ (segmentPlansResult.segmentPlans) $ \SpecSegmentPlan {..} -> do
    let docFileFp = FS.toFilePath cfg segmentFileName
        startIdx = max 1 startLineNum
        endIdx = min lineCount endLineNum
    if startIdx <= endIdx
      then do
        let segmentText =
              T.unlines
                $ take (endIdx - startIdx + 1)
                $ drop (startIdx - 1) allSpecLines
        liftIO $ FS.clearFileOnDisk docFileFp
        appendRes <- liftIO $ FS.appendToFile docFileFp segmentText
        case appendRes of
          Left err ->
            throwError
              $ "Error writing doc segment "
              <> segmentFileName
              <> ": "
              <> err
          Right () -> pure ()
      else do
        putTextLn
          $ "Warning: Invalid line range for "
          <> segmentFileName
          <> " ("
          <> show startLineNum
          <> " to "
          <> show endLineNum
          <> ")"
        liftIO $ FS.clearFileOnDisk docFileFp
        appendRes <-
          liftIO
            $ FS.appendToFile docFileFp
            $ "[No lines, invalid start/end range]\n"
            <> segmentTitle
        case appendRes of
          Left err ->
            throwError
              $ "Error writing doc segment "
              <> segmentFileName
              <> ": "
              <> err
          Right () -> pure ()

  -- 4) Gather doc files + existing files
  ignoredDirs <- BS.getIgnoredDirs @bs
  existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs (configBaseDir cfg)
  modify' (updateExistingFiles existingFileNames)

  -- 5) Architecture design
  let archPrompt = makeArchitectureDesignPrompt <> " Your architecture should ideally break it into relatively independent components corresponding to the different sub-sections of the spec to minimise the amount of spec that needs to be included in the context."
      archCtxt =
        makeBaseContext
          ( projectTexts.projectSummaryText
              <> "\nWe have these doc files describing different parts of the spec:\n"
              <> Tools.toJ (segmentPlansResult.segmentPlans)
          )
          archPrompt
      exampleArch =
        ThingWithDescription
          "Overall architecture referencing sub-spec doc files..."
      archRunner () =
        Engine.runAiFunc @bs
          archCtxt
          HighIntelligenceRequired
          readOnlyTools
          exampleArch
          validateAlwaysPass
          (configTaskMaxFailures cfg)

  plannedArch <- memoise (configCacheDir cfg) "architecture" () (const "") archRunner

  -- 6) Code file planning
  let background =
        projectTexts.projectSummaryText
          <> "\nThe architecture will be as follows:\n"
          <> plannedArch.description
          <> "\nWe have these doc files for reference:\n"
          <> Tools.toJ (segmentPlansResult.segmentPlans)

      filePlanCtxt = makeBaseContext background makeFilenamesPrompt
      examplePlannedFiles =
        ThingsWithDependencies
          { items =
              [ ThingWithDependencies
                  "someFile.go"
                  "Handles lines 1..100 from http_spec_part1.txt"
                  ["http_spec_part1.txt"],
                ThingWithDependencies
                  "someOtherFile.go"
                  "Handles lines 101..200 from http_spec_part2.txt"
                  ["http_spec_part2.txt"]
              ]
          }

      runner () =
        Engine.runAiFunc @bs
          filePlanCtxt
          HighIntelligenceRequired
          readOnlyTools
          examplePlannedFiles
          validateFileNamesNoNestedPaths
          (configTaskMaxFailures cfg)

  plannedFiles <- memoise (configCacheDir cfg) "file_planner" () (const "") runner

  -- 7) Create each file
  forM_ plannedFiles (makeFile @bs filePlanCtxt.contextBackground [])

  putTextLn "Finished creating project based on spec!"

fixFailedTestsAndCompilationSimple ::
  forall bs.
  (BS.BuildSystem bs) =>
  Text ->
  AppM ()
fixFailedTestsAndCompilationSimple background = do
  cfg <- ask
  origSt <- get
  sourceFileNames <- filterM (BS.isBuildableFile @bs) $ map existingFileName origSt.stateFiles
  case sourceFileNames of
    [] -> return ()
    (someSourceFile : _) -> do
      _ <- Tools.considerBuildAndTest @bs someSourceFile
      st <- get
      let res = stateCompileTestRes st
          mayTask = case (compileRes res, testRes res) of
            (Nothing, Nothing) -> Nothing
            (Just compileErr, _) -> Just $ "The project fails to build, please fix it. The error: \n" <> compileErr
            (Nothing, Just testErr) -> Just $ "Fix the error that occurred building or running the tests. At each step please append to the journal.txt what you're currently doing and what you plan to do next. The error: \n" <> testErr
      case mayTask of
        Nothing -> do
          putTextLn $ "Tests and compilation are fine, no need to fix"
          return ()
        Just task -> do
          let exampleUnitTestDone = UnitTestDone True
              ctxt = makeBaseContext background ("YOUR CURRENT TASK: " <> task)
          putTextLn $ "Running test fix: " <> task
          Engine.runAiFunc @bs ctxt MediumIntelligenceRequired allTools exampleUnitTestDone validateUnitTest (configTaskMaxFailures cfg)
          return ()

-- | A plan of how to fix each file that is failing compilation/tests
data FailingFilePlan = FailingFilePlan
  { failingFileName :: Text,
    -- | Why it fails (taken from or inferred from compile/test messages)
    failingFileReason :: Text,
    -- | Other files that may need to be opened/fixed
    failingFileDependencies :: [Text],
    -- | A textual plan or summary of how the fix should proceed
    fixPlan :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON FailingFilePlan

instance FromJSON FailingFilePlan

-- | The top-level JSON structure listing all failing files
data FailingFilesPlan = FailingFilesPlan
  { failingFiles :: [FailingFilePlan]
  }
  deriving (Show, Eq, Generic)

instance ToJSON FailingFilesPlan

instance FromJSON FailingFilesPlan

-- | When we ask the LLM to fix a single file, it returns yes/no plus a rationale.
--   We'll use a validator that checks the real compile/test results to see if
--   references to that file remain.
data SingleFileFixResult = SingleFileFixResult
  { fileFixConfirmed :: Bool,
    rationale :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SingleFileFixResult

instance FromJSON SingleFileFixResult

--------------------------------------------------------------------------------
-- Validator: Checking that a single file is fixed
--------------------------------------------------------------------------------

-- | This validator re-runs the entire build+test suite, then
--   checks whether the error logs still mention @fileName@.
--   If yes, we fail => the LLM gets re-prompted to fix the file.
--   If no, we pass => continue to the next failing file.
validateSingleFileFix ::
  forall bs.
  (BS.BuildSystem bs) =>
  -- | The file we’re trying to fix
  Text ->
  SingleFileFixResult ->
  AppM (Either (MsgKind, Text) SingleFileFixResult)
validateSingleFileFix fileName userRes = do
  -- Re-run build and tests (pick a buildable file if you like):
  _ <- Tools.considerBuildAndTest @bs fileName
  st <- get
  let cErr = compileRes (stateCompileTestRes st)
      tErr = testRes (stateCompileTestRes st)
  case (cErr, tErr) of
    (Nothing, Nothing) ->
      -- No errors at all => definitely no mention of 'fileName'.
      pure $ Right userRes
    (Just err, _) -> pure $ Left (OtherMsg, "Compilation is still failing; error is: " <> err)
    _ ->
      let allErr = fromMaybe "" cErr <> "\n" <> fromMaybe "" tErr
       in if T.isInfixOf (" " <> fileName) allErr
            then
              pure
                $ Left
                  ( OtherMsg,
                    "Still seeing an error referencing "
                      <> fileName
                      <> ". The build/test errors are:\n"
                      <> allErr
                  )
            else
              -- Some other file is still failing, but *this* file isn't
              -- mentioned => we consider this file "fixed."
              pure $ Right userRes

fixFailedTestsAndCompilation ::
  forall bs.
  (BS.BuildSystem bs) =>
  -- | Some background or context that the LLM should always see
  Text ->
  -- | Relevant dependencies
  [Text] ->
  AppM ()
fixFailedTestsAndCompilation background relevantFiles = do
  cfg <- ask

  -- 1) First check if everything is OK
  --    (pick any buildable file or an existing main file, etc.)
  _ <- Tools.considerBuildAndTest @bs "main.go"
  st <- get
  let res = stateCompileTestRes st
      cErr = compileRes res
      tErr = testRes res

  case (cErr, tErr) of
    (Nothing, Nothing) -> do
      putTextLn "Compilation and all tests are already passing. Nothing to fix."
      pure ()
    _ -> do
      let errorsCombined =
            "Compilation error:\n"
              <> fromMaybe "" cErr
              <> "\nTest error:\n"
              <> fromMaybe "" tErr

      -- 2) Ask the LLM to produce a plan listing the failing files
      let planContext =
            makeBaseContext background
              $ "YOUR CURRENT TASK: fixing compilation/test failures. "
              <> "We have the following errors:\n"
              <> errorsCombined
              <> "\n\nPlease identify which files are failing (don't compile, or have a unit test that fails to pass)."
              <> "For each failing file, list:\n"
              <> "- failingFileName\n"
              <> "- failingFileReason (how or why it fails)\n"
              <> "- failingFileDependencies (other files we may need to open to fix it)\n"
              <> "- fixPlan (the approach to fix that file)\n\n"
              <> "Return them as JSON in the required format. For dependencies, remember to "
              <> "include documentation/specification files that may be useful. "
              <> "Note that it's possible some existing tests may be wrong; always check the spec to make sure the test is testing for the correct behaviour."

          examplePlan =
            FailingFilesPlan
              { failingFiles =
                  [ FailingFilePlan
                      { failingFileName = "main.go",
                        failingFileReason = "Possible syntax error on line 42",
                        failingFileDependencies = ["main_test.go", "utility.go"],
                        fixPlan = "We will correct the syntax and then update the test."
                      }
                  ]
              }
      modify' clearOpenFiles
      forM_ (journalFileName : relevantFiles) $ \dep ->
        Tools.openFile dep cfg
      plan <-
        Engine.runAiFunc @bs
          planContext
          HighIntelligenceRequired
          readOnlyTools
          examplePlan
          validateAlwaysPass
          (configTaskMaxFailures cfg)

      -- 3) For each failing file, attempt a fix
      forM_ (plan.failingFiles) $ \fPlan -> do
        modify' clearOpenFiles

        -- open the failing file + dependencies
        forM_ (fPlan.failingFileName : fPlan.failingFileDependencies ++ (journalFileName : relevantFiles)) $ \dep ->
          Tools.openFile dep cfg

        let fixContext =
              makeBaseContext background
                $ "YOUR CURRENT TASK: fixing compilation/test failures. "
                <> "File to fix: "
                <> fPlan.failingFileName
                <> "\nReason: "
                <> fPlan.failingFileReason
                <> "\nProposed approach: "
                <> fPlan.fixPlan
                <> "\n\nPlease implement the fix now. Write the approach you take to the journal for future reference, with particular emphasis on any assumptions you're making (to avoid cycles of some test fixes breaking other tests that make different assumptions), and note the spec should be the main source of truth, followed by real integration test behaviour. Don't mention trivial things like code change details/fixes, only mention changes in logic/behaviour or assumptions."

            exampleFixConfirmation =
              SingleFileFixResult
                { fileFixConfirmed = True,
                  rationale = "Corrected syntax; test now passes."
                }

        -- This will be re-called if the file’s errors still appear.
        putTextLn $ "Attempting to fix " <> fPlan.failingFileName
        _ <-
          Engine.runAiFunc @bs
            fixContext
            MediumIntelligenceRequired
            -- Tools for editing/writing code:
            allTools
            exampleFixConfirmation
            (validateSingleFileFix @bs fPlan.failingFileName)
            (configTaskMaxFailures cfg)
        return ()

      putTextLn "Done fixing all files that were identified!"
