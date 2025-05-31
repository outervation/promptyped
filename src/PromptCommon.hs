{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PromptCommon where

import BuildSystem as BS
import Control.Monad.Except
import TerminalInput qualified
import Core
import Data.Aeson as AE
import Data.Graph
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.List qualified as L
import Engine qualified
import FileSystem qualified as FS
import Memoise (memoise)
import PromptTexts
import Relude
import System.Directory qualified as DIR
import Tools qualified


writeFileIfDoesntExist :: Text -> Text -> AppM ()
writeFileIfDoesntExist fName fContents = do
  cfg <- ask
  let fPath = FS.toFilePath cfg fName
  alreadyExistsOnDisk <- liftIO $ FS.fileExistsOnDisk fPath
  unless alreadyExistsOnDisk
    $ liftIO (FS.appendToFile fPath fContents)
    >>= \case
      Left err -> throwError $ "Error writing to " <> T.pack fPath <> ": " <> err
      _ -> return ()

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

lookupFileProposedChanges :: Text -> FilesProposedChanges -> Maybe FileProposedChanges
lookupFileProposedChanges targetFileName container =
  find (\fpc -> fileName fpc == targetFileName) (filesProposedChanges container)

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
  -- 1. Build a mapf rom fileName -> ThingWithDependencies
  let fileMap :: Map Text ThingWithDependencies
      fileMap = Map.fromList [(f.name, f) | f <- files]

  -- 2. Check every dependency to ensure it exists in fileMap
  forM_ files $ \pf -> do
    forM_ pf.dependencies $ \dep ->
      unless (Map.member dep fileMap || alreadyExists dep)
        $ Left
        $ "Error: File "
        <> pf.name
        <> " depends on non-existing item: "
        <> dep
        <> ". Note that only items we create need to be mentioned, not external items/libs like Boost etc."

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
       in Left $ "Error: Cycle detected in dependencies: " <> show cycleNames

  -- 5. Perform the topological sort
  let sortedVertices = topSort graph

  -- 6. Convert each 'Vertex' back into the original 'ThingWithDependencies'
  let sortedFiles = map (\v -> let (f, _, _) = nodeFromVertex v in f) sortedVertices

  pure $ reverse sortedFiles

validatePropertyOfThingsWithDependencies :: ThingsWithDependencies -> (ThingWithDependencies -> Maybe Text) -> Either (MsgKind, Text) ThingsWithDependencies
validatePropertyOfThingsWithDependencies (ThingsWithDependencies things) validator =
  case mapMaybe validator things of
    [] -> Right $ ThingsWithDependencies things
    errors -> Left (OtherMsg, T.intercalate ", " errors)

validatePathNotNested :: ThingWithDependencies -> Maybe Text
validatePathNotNested thing =
  if T.isInfixOf "/" thing.name
    then Just $ "Error; nested paths are not allowed, but " <> thing.name <> " is a nested path."
    else Nothing

validateThingsWithDependencies :: Context -> ThingsWithDependencies -> AppM (Either (MsgKind, Text) [ThingWithDependencies])
validateThingsWithDependencies _ pf = do
  st <- get
  return $ either (\x -> Left (OtherMsg, x)) Right $ topologicalSortThingsWithDependencies (stateFiles st) pf

validateThingsWithDependenciesContainsAll ::
  [Text] ->
  Context ->
  ThingsWithDependencies ->
  AppM (Either (MsgKind, Text) ())
validateThingsWithDependenciesContainsAll requiredNames _ things =
  let availableNamesSet :: Set Text
      availableNamesSet = Set.fromList $ fmap name (items things)

      missingNames :: [Text]
      missingNames = filter (\reqName -> not (Set.member reqName availableNamesSet)) requiredNames
   in if null missingNames
        then pure $ Right ()
        else
          let missingNamesStr :: Text
              missingNamesStr = T.intercalate ", " missingNames
              errorMessage :: Text
              errorMessage = "The following required names were not found: " <> missingNamesStr
           in pure $ Left (OtherMsg, errorMessage)

validateFileNamesNoNestedPaths :: Context -> ThingsWithDependencies -> AppM (Either (MsgKind, Text) [ThingWithDependencies])
validateFileNamesNoNestedPaths ctxt things = do
  case validatePropertyOfThingsWithDependencies things validatePathNotNested of
    Left err -> pure $ Left err
    Right things' -> validateThingsWithDependencies ctxt things'

data ThingWithDescription = ThingWithDescription
  { description :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON ThingWithDescription

instance FromJSON ThingWithDescription

validateAlwaysPassIfCompileTestsFine :: Context -> a -> AppM (Either (MsgKind, Text) a)
validateAlwaysPassIfCompileTestsFine _ x = second (const x) <$> checkCompileTestResults

validateFileModifiedFine :: Text -> Context -> a -> AppM (Either (MsgKind, Text) a)
validateFileModifiedFine fileName ctxt x = do
  case hasFileBeenModified ctxt fileName of
    False -> pure $ Left (OtherMsg, "Error: no modifications have been made to " <> fileName <> ". If you're very confident the desired change is already present in the file, then just make a small edit to the file to add a comment, and this validation will pass.")
    True -> second (const x) <$> checkCompileTestResults

data AiChangeVerificationResult = AiChangeVerificationResult
  { changeAlreadyBeenMade :: Bool,
    messageToLlm :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON AiChangeVerificationResult

instance FromJSON AiChangeVerificationResult

validateAiChangeVerificationResult :: Context -> AiChangeVerificationResult -> AppM (Either (MsgKind, Text) AiChangeVerificationResult)
validateAiChangeVerificationResult _ x = do
  if T.length x.messageToLlm == 0 && not x.changeAlreadyBeenMade
    then pure $ Left (OtherMsg, "Error: You must return a non-empty messageToLLm when you claim the change it was tasked with has not been made")
    else pure $ Right x

validateFileModifiedWithAi :: forall bs a. (BS.BuildSystem bs) => Text -> Context -> a -> AppM (Either (MsgKind, Text) a)
validateFileModifiedWithAi fileName origCtxt x = do
  cfg <- ask
  let tools = [Tools.ToolReturn, Tools.ToolPanic]
      exampleReturn = AiChangeVerificationResult True "Task completed successfuly!"
      background = "You are an AI agent responsible for checking other agents' output."
      task = "You are reponsible for determining whether the below file modification task was indeed done to file " <> fileName <> ". Please do so based on what you can see in the file, and return the result in the format specified, with \"changeAlreadyBeenMade\": true if you see the task has indeed been done, and false if it's clear that the requested file modification hasn't been made, along with a \"messagToLlm\", that in the case the change hasn't been made should inform the LLM why the task it claims to have been completed has not been done. Don't check for correctness, i.e. an implementation of the change with a small bug or two should still pass, as they'll be handled later (however you should reject a change that does something obviously stupid). Do however check that the change is actually present, not just that it left a comment saying it did the change. For a refactoring task make sure it's done completely, with nothing left to do, not partially. If it did more than asked, that's okay as long as the changes it made looks reasonably helpful/necessary towards achieving the task. Note that for verifying Go dependencies are added, the base dependency will appear as 'indirect' in go.mod, and the full dependency won't be shown there util the dependency is actually imported and used in a file, so don't expect it to be. The task that was supposed to have been done:\n" <> origCtxt.contextTask
      ctxt = makeBaseContext background task
  verificationRes <- Engine.runAiFunc @bs ctxt MediumIntelligenceRequired tools exampleReturn validateAiChangeVerificationResult (configTaskMaxFailures cfg)
  liftIO $ putTextLn $ "Got result from AI check of modification:\n" <> origCtxt.contextTask <> "\nResult is: " <> show verificationRes
  pure $ case verificationRes.changeAlreadyBeenMade of
    True -> Right x
    False -> Left (OtherMsg, "Error: An LLM determined that the change you claim to have been made has not actually been made: " <> verificationRes.messageToLlm)

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

validateCreatedFiles :: Context -> CreatedFiles -> AppM (Either (MsgKind, Text) [CreatedFile])
validateCreatedFiles _ cf = do
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

data FileUnitTests = FileUnitTests
  { unitTestFileName :: Text,
    unitTests :: [UnitTest]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON FileUnitTests

instance FromJSON FileUnitTests

data UnitTests = UnitTests
  { unitTestFiles :: [FileUnitTests]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON UnitTests

instance FromJSON UnitTests

validateUnitTests :: Context -> UnitTests -> AppM (Either (MsgKind, Text) UnitTests)
validateUnitTests _ cf = pure $ Right cf

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
    (Just _, _) -> Left (CompileFailMsg, "Error, compilation failed, fix it before returning. Note that if you see a 'missing import path' compilation error, it may be because you forgot a closing ')' for the go import list. If you see 'is not a package path' when trying to import a local file you created, remember you should include 'project_name/filename', NOT '/home/username/project_name/filename' or 'username/project_name/filename'. The compilation error is described earlier above.")
    (Nothing, Just _) -> Left (TestFailMsg, "Error, unit tests didn't all pass (or failed to compile), fix them first. I encourage you to add more logging/printf for debugging if necessary, and to record your current step and planned future steps in the journal.txt . If you see 'is not a package path' when trying to import a local file you created into a test, remember you should include 'project_name/filename', NOT '/home/username/project_name/filename' or 'username/project_name/filename'. The error with unit tests is described earlier above.")

validateUnitTest :: Context -> UnitTestDone -> AppM (Either (MsgKind, Text) ())
validateUnitTest _ t = case unitTestPassedSuccessfully t of
  False -> pure $ Left (OtherMsg, "Your return value of false indicates it didn't pass successfully")
  True -> checkCompileTestResults

clearJournal :: AppM ()
clearJournal = do
  cfg <- ask
  liftIO $ FS.clearFileOnDisk (FS.toFilePath cfg journalFileName)

makeUnitTestsInner :: forall bs. (BS.BuildSystem bs) => Text -> Text -> (Text -> Text) -> AppM ()
makeUnitTestsInner background fileName makeTestPrompt = do
  cfg <- ask
  let unitTestExampleFileName = T.replace ".go" "_test.go" fileName
      unitTestExampleFilePath = FS.toFilePath cfg unitTestExampleFileName
  unitTestExists <- liftIO $ FS.fileExistsOnDisk unitTestExampleFilePath
  Tools.openFile @bs Tools.DoFocusOpenedFile fileName cfg
  when unitTestExists $ Tools.openFile @bs Tools.DoFocusOpenedFile unitTestExampleFileName cfg
  let makeCtxt task = makeBaseContext background task
      exampleUnitTests =
        UnitTests
          [ FileUnitTests
              { unitTestFileName = unitTestExampleFileName,
                unitTests =
                  [ UnitTest "testSomethingWorks" "Should test that ...",
                    UnitTest "testsSomethingelseWorks" "Should test that ..."
                  ]
              }
          ]
      runner fileName' = Engine.runAiFunc @bs (makeCtxt $ makeTestPrompt fileName') HighIntelligenceRequired Engine.allTools exampleUnitTests validateUnitTests (configTaskMaxFailures cfg)
  planRes <- memoise (configCacheDir cfg) "test_planner" fileName id runner
  forM_ (unitTestFiles planRes) $ \fileUnitTests -> do
    let testFileName = unitTestFileName fileUnitTests
    Tools.openFile @bs Tools.DoFocusOpenedFile testFileName cfg
    let exampleUnitTestDone = UnitTestDone True
    let makeUnitTest UnitTest {..} = Engine.runAiFunc @bs (makeCtxt $ makeUnitTestPrompt fileName testFileName unitTestName unitTestSummary) MediumIntelligenceRequired Engine.allTools exampleUnitTestDone validateUnitTest (configTaskMaxFailures cfg)
    forM_ fileUnitTests.unitTests $ \test -> do
      memoise (configCacheDir cfg) ("test_creator_" <> testFileName <> "_" <> test.unitTestName) test unitTestName makeUnitTest
      liftIO $ putTextLn $ "Made test " <> test.unitTestName <> " for " <> testFileName
    modify' (closeOpenFile testFileName)

makeUnitTests :: forall bs. (BS.BuildSystem bs) => Text -> ThingWithDependencies -> AppM ()
makeUnitTests background plannedFile = do
  let fileName = plannedFile.name
  cfg <- ask
  clearJournal
  modify' clearOpenFiles
  let dependencies = [fileName, journalFileName] ++ plannedFile.dependencies
  forM_ dependencies $ \x -> Tools.openFile @bs Tools.DoFocusOpenedFile x cfg
  makeUnitTestsInner @bs background fileName makeUnitTestsPrompt

makeFile :: forall bs. (BS.BuildSystem bs) => Text -> [Text] -> ThingWithDependencies -> AppM ()
makeFile background extraFiles pf = do
  cfg <- ask
  resetCompileTestState
  modify' clearOpenFiles
  let dependencies = [pf.name, journalFileName] ++ pf.dependencies ++ extraFiles
  forM_ dependencies $ \x -> Tools.openFile @bs Tools.DoFocusOpenedFile x cfg
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
    let runner fileName = Engine.runAiFunc @bs (makeCtxt fileName) MediumIntelligenceRequired Engine.allTools exampleCreatedFiles validateCreatedFiles (configTaskMaxFailures cfg)
    createdFiles <- memoise (configCacheDir cfg) "file_creator" pf.name id runner
    forM_ createdFiles $ \x -> modify' $ updateFileDesc (createdFileName x) (createdFileSummary x)
    makeUnitTests @bs background pf

data AutoRefactorUnitTests = DoAutoRefactorUnitTests | DontAutoRefactorUnitTests
  deriving (Eq, Ord, Show)

makeRefactorFileTask :: forall bs. (BS.BuildSystem bs) => Text -> [(ExistingFile, Tools.FocusOpenedFile)] -> Text -> [ThingWithDependencies] -> AutoRefactorUnitTests -> AppM ()
makeRefactorFileTask background initialDeps fileName desiredChanges refactorUnitTests = do
  cfg <- ask
  -- _ <- Tools.buildAndTest @bs
  modify' clearOpenFiles
  -- Note: file opened first will appear last (most recent)
  let dependencies = [(fileName, Tools.DoFocusOpenedFile), (journalFileName, Tools.DoFocusOpenedFile)] ++ map (\(x, shouldFocus) -> (x.existingFileName, shouldFocus)) initialDeps
  forM_ dependencies $ \(x, shouldFocus) -> Tools.openFile @bs shouldFocus x cfg
  Tools.forceFocusFile fileName
  closeIrrelevantUnfocusedFiles @bs background desiredChanges fileName
  let makeChange description = do
        let ctxt = makeBaseContext background $ "Your task is to refactor the file " <> fileName <> " to make the change: " <> (summary description) <> ". Do NOT make other changes yet. If the build/tests are currently broken, fix those first. If the change has already been made, you can return directly."
            exampleChange = ModifiedFile "someFile.go" "Update the file to add ... so that it ..."
            validateChange = Engine.combineValidatorsSameRes validateAlwaysPassIfCompileTestsFine (validateFileModifiedWithAi @bs fileName)
        liftIO $ putTextLn $ "Running file modification task: " <> show description
        Engine.runAiFunc @bs ctxt MediumIntelligenceRequired Engine.allTools exampleChange validateChange (configTaskMaxFailures cfg)
  modifications <- forM desiredChanges $ \x -> do
    modification <- memoise (configCacheDir cfg) ("file_modifier_" <> fileName) x (\desc -> desc.name) makeChange
    putTextLn $ "Done task: " <> show x
    return $ "Intended modification: " <> x.summary <> ", with model describing what it did as " <> show modification <> "."
  let modificationsTxt = "The model made the following changes: \n" <> T.unlines modifications
  when ((refactorUnitTests == DoAutoRefactorUnitTests) && not (T.isInfixOf "_test.go" fileName))
    $ makeUnitTestsInner @bs background fileName
    $ makeUnitTestsForSpecificChangePrompt modificationsTxt

data BigRefactorConfig = BigRefactorConfig
  { bigRefactorInitialOpenFiles :: [Text],
    bigRefactorOverallTask :: Text,
    bigRefactorOverallTaskShortName :: Text,
    bigRefactorDoFinalSort :: Bool,
    bigRefactorSpecFiles :: [Text],
    bigRefactorDeliberatelyPromptForTestRefactors :: Bool
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
        forM_ ([fileName, journalFileName] ++ refactorCfg.bigRefactorInitialOpenFiles) $ \x -> Tools.openFile @bs Tools.DoFocusOpenedFile x cfg
  existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs cfg.configBaseDir
  modify' (updateExistingFiles existingFileNames)
  st <- get
  let filterSourceFile x = do
        buildable <- BS.isBuildableFile @bs x
        return $ buildable && not (T.isInfixOf "_test.go" x)
  sourceFileNames <- filterM filterSourceFile $ map existingFileName st.stateFiles
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
        Engine.runAiFunc @bs ctxt MediumIntelligenceRequired (Tools.ToolAppendFile : Engine.readOnlyTools) exampleProposedChanges validateThingsWithDependencies (configTaskMaxFailures cfg)
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
      refineChangesTask () = Engine.runAiFunc @bs combineCtxt HighIntelligenceRequired (Tools.ToolAppendFile : Engine.readOnlyTools) combineExample Engine.validateAlwaysPass (configTaskMaxFailures cfg)
  modify' clearOpenFiles
  forM_ (journalFileName : refactorCfg.bigRefactorInitialOpenFiles) $ \x -> Tools.openFile @bs Tools.DoFocusOpenedFile x cfg
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
      getExtraFilesTask () = Engine.runAiFunc @bs extraFilesCtxt HighIntelligenceRequired Engine.readOnlyTools exampleExtraFiles validateFileNamesNoNestedPaths (configTaskMaxFailures cfg)
  extraFilesNeeded <- memoise (configCacheDir cfg) "all_extra_files" () (const "") getExtraFilesTask
  let makeFileBackground = background <> "\n You are currently working on adding some extra files that are necessary as part of the refactoring."
  forM_ extraFilesNeeded (makeFile @bs makeFileBackground refactorCfg.bigRefactorInitialOpenFiles)
  let relFiles = map (\x -> x.name) extraFilesNeeded ++ map (\x -> x.fileName) plannedTasksRefined.filesProposedChanges
      getFileDependenciesCtxt =
        makeBaseContext background
          $ "You've previously just produced a list per file of tasks that must be done to refactor each file to achieve the above objective ("
          <> objectiveShortName
          <> ") as well as some extra files that must be created."
          <> "Now you'll need to create a list of files and their file dependencies, so we know in which order we need to modify the files (and which relevant files should be open when modifying each file)."
          <> "The previously created tasks are: \n"
          <> Tools.toJ plannedTasksRefined
          <> "The previously created extra files are: \n"
          <> Tools.toJ extraFilesNeeded
          <> "\n So overall the files you need to identify dependencies for are "
          <> show relFiles
      getFileDependenciesValidator = Engine.combineValidators (validateThingsWithDependenciesContainsAll relFiles) validateThingsWithDependencies
      getFileDependenciesExample =
        ThingsWithDependencies
          { items =
              [ ThingWithDependencies "someFile.go" "This file contains functionality for... (this part isn't important here as it's not used)" [],
                ThingWithDependencies "someOtherFile.go" "This file contains functionality for something different...  (this part isn't important here as it's not used)" ["someFile.go"]
              ]
          }
      getFileDependenciesTask () = Engine.runAiFunc @bs getFileDependenciesCtxt HighIntelligenceRequired Engine.readOnlyTools getFileDependenciesExample getFileDependenciesValidator (configTaskMaxFailures cfg)
  fileDependencies <- memoise (configCacheDir cfg) "all_file_final_dependencies" () (const "") getFileDependenciesTask
  let docDeps = map (`ExistingFile` "") refactorCfg.bigRefactorInitialOpenFiles
  forM_ fileDependencies $ \x -> do
    let depsFiles = map (`ExistingFile` "") x.dependencies ++ docDeps
        deps = map (\f -> (f, Tools.DoFocusOpenedFile)) depsFiles
    case lookupFileProposedChanges x.name plannedTasksRefined of
      Nothing -> pure ()
      Just changes -> makeRefactorFileTask @bs background deps changes.fileName changes.proposedChanges DoAutoRefactorUnitTests

data RelevantFiles = RelevantFiles { relevantFileNames :: [Text] }
  deriving (Generic, Eq, Ord, Show)
instance ToJSON RelevantFiles
instance FromJSON RelevantFiles

makeTargetedRefactorFilesProject :: forall bs. (BS.BuildSystem bs) => ProjectTexts -> BigRefactorConfig -> AppM ()
makeTargetedRefactorFilesProject projectTexts refactorCfg = do
  cfg <- ask
  ignoredDirs <- BS.getIgnoredDirs @bs
  existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs cfg.configBaseDir
  modify' (updateExistingFiles existingFileNames)

  st <- get
  let filterSourceFile x = do
        buildable <- BS.isBuildableFile @bs x
        isTest <- BS.isTestFile @bs x
        return $ buildable && not isTest
  allSourceFileNames <- filterM filterSourceFile $ map Core.existingFileName st.stateFiles

  -- 1. Open all source files unfocused, spec files unfocused, journal and initial open files focused
  liftIO $ putTextLn "Opening all source files (unfocused), spec files (unfocused), and journal/initial deps (focused)..."
  modify' clearOpenFiles
  forM_ allSourceFileNames $ \fileName -> Tools.openFile @bs Tools.DontFocusOpenedFile fileName cfg
  forM_ refactorCfg.bigRefactorSpecFiles $ \fileName -> Tools.openFile @bs Tools.DontFocusOpenedFile fileName cfg -- spec files
  Tools.openFile @bs Tools.DoFocusOpenedFile journalFileName cfg
  forM_ refactorCfg.bigRefactorInitialOpenFiles $ \fileName -> Tools.openFile @bs Tools.DoFocusOpenedFile fileName cfg

  -- Helper to get all known file names for validation (project files + initial open files + spec files)
  let getAllKnownValidFileNames :: AppM (Set Text)
      getAllKnownValidFileNames = do
        st_local <- get
        pure $ Set.fromList $
          (map Core.existingFileName (stateFiles st_local)) ++
          refactorCfg.bigRefactorInitialOpenFiles ++
          refactorCfg.bigRefactorSpecFiles

  -- Prepare background for LLM
  let task = refactorCfg.bigRefactorOverallTask
      objectiveShortName = refactorCfg.bigRefactorOverallTaskShortName
      refactorBackground = makeRefactorBackgroundPrompt task
      background = projectTexts.projectSummaryText <> "\n" <> refactorBackground
      maxFocused = cfg.configMaxNumFocusedFiles

  -- 2. Identify Relevant Files for Initial Focus (LLM Call 1)
  let relevantFilesCtxt =
        makeBaseContext background
          ( "Given the overall objective (" <> objectiveShortName <> ": " <> task <> "),"
              <> " and considering all source files are currently open (most unfocused, some like '" <> journalFileName <> "' and initial dependencies might be focused), please identify a list of at most " <> show maxFocused <> " existing filenames that are most relevant and should be focused for closer inspection (including any already-focused files that should still be focused). These files will be brought into focus to aid in detailed planning. "
              <> "Return a JSON object with a single key 'relevantFileNames' containing a list of these filenames; it may be empty if there are no existing files yet. "
              <> "The available source files are: " <> T.intercalate ", " allSourceFileNames <> "."
              <> (if null refactorCfg.bigRefactorInitialOpenFiles
                  then ""
                  else " Consider also these initially opened files: " <> T.intercalate ", " refactorCfg.bigRefactorInitialOpenFiles <> ".")
              <> (if null refactorCfg.bigRefactorSpecFiles
                  then ""
                  else " Additionally, the following specification files are open (unfocused) and may provide important context: " <> T.intercalate ", " refactorCfg.bigRefactorSpecFiles <> ".")
          )

  let exampleFocusNames = L.nub $
        take (min 2 (length allSourceFileNames)) allSourceFileNames ++
        take (min 1 (length refactorCfg.bigRefactorSpecFiles)) refactorCfg.bigRefactorSpecFiles
  let exampleRelevantFiles = RelevantFiles { relevantFileNames = take maxFocused exampleFocusNames }


  let validateRelevantFiles :: Context -> RelevantFiles -> AppM (Either (MsgKind, Text) RelevantFiles)
      validateRelevantFiles _ rfs = do
        knownFileNames <- getAllKnownValidFileNames
        let missing = filter (not . (`Set.member` knownFileNames)) (relevantFileNames rfs)
        if null missing
        then pure $ Right rfs
        else pure $ Left (OtherMsg, "The following relevant files identified by the LLM for initial focus do not exist: " <> T.intercalate ", " missing)

  initialFocusResult <- memoise (configCacheDir cfg) ("relevant_files_for_refactor_" <> objectiveShortName) () (const "") $ \_ ->
    Engine.runAiFunc @bs relevantFilesCtxt HighIntelligenceRequired Engine.readOnlyTools exampleRelevantFiles validateRelevantFiles (configTaskMaxFailures cfg)
  
  let filesForInitialFocus = relevantFileNames initialFocusResult
  liftIO $ putTextLn $ "LLM identified " <> T.pack (show $ length filesForInitialFocus) <> " files for initial focus: " <> T.intercalate ", " filesForInitialFocus

  -- 3. Focus all files identified for initial inspection
  liftIO $ putTextLn $ "Focusing all " <> T.pack (show $ length filesForInitialFocus) <> " files selected for initial detailed inspection..."
  forM_ filesForInitialFocus $ \ffName -> do
      mOpenFile <- getOpenFile ffName <$> get
      unless (isJust mOpenFile) $ do
          liftIO $ putTextLn $ "File " <> ffName <> " (for initial focus) was not in the open list. Opening it now."
          Tools.openFile @bs Tools.DontFocusOpenedFile ffName cfg -- Open it if somehow not opened, though it should be
      Tools.forceFocusFile ffName -- This will make it focused.
  
  stAfterInitialFocus <- get
  let initiallyFocusedFileNames = Core.focusedFileNames stAfterInitialFocus
  liftIO $ putTextLn $ "Files currently focused after initial batch: " <> T.intercalate ", " initiallyFocusedFileNames

  -- 4. Identify Files to Actually Modify (LLM Call 2)
  let filesToModifyPromptText =
        "Given the overall objective (" <> objectiveShortName <> ": " <> task <> "),"
        <> (if null initiallyFocusedFileNames
            then " and considering all available source files (" <> T.intercalate ", " allSourceFileNames <> "), "
            else " and that the following files are currently focused for inspection: " <> T.intercalate ", " initiallyFocusedFileNames <> ", ")
        <> "please identify a list of filenames that *actually need to be modified* to achieve the objective. "
        <> "This list can be empty if no direct modifications to existing files are needed (e.g., the task only involves adding new files). "
        <> "The available source files for modification consideration include: " <> T.intercalate ", " allSourceFileNames <> "."
        <> (if null refactorCfg.bigRefactorInitialOpenFiles
            then ""
            else " Also consider these initially opened files: " <> T.intercalate ", " refactorCfg.bigRefactorInitialOpenFiles <> ".")
        <> (if null refactorCfg.bigRefactorSpecFiles
            then ""
            else " The following specification files are also open (unfocused) and might inform which files need changes or provide context: " <> T.intercalate ", " refactorCfg.bigRefactorSpecFiles <> ".")
        <> " Return a JSON object with a single key 'relevantFileNames' containing a list of these filenames (use 'relevantFileNames' as the key, as these are files relevant for modification)."

  let filesToModifyCtxt = makeBaseContext background filesToModifyPromptText
  
  let exampleFilesToModifyNames = 
        if not (null initiallyFocusedFileNames) then take (min 1 (length initiallyFocusedFileNames)) initiallyFocusedFileNames
        else if not (null allSourceFileNames) then take (min 1 (length allSourceFileNames)) allSourceFileNames
        else []
  let exampleFilesToModify = RelevantFiles { relevantFileNames = exampleFilesToModifyNames }

  let validateFilesToModify :: Context -> RelevantFiles -> AppM (Either (MsgKind, Text) RelevantFiles)
      validateFilesToModify _ rfs = do
        knownFileNames <- getAllKnownValidFileNames
        let missing = filter (not . (`Set.member` knownFileNames)) (relevantFileNames rfs)
        if null missing
        then pure $ Right rfs
        else pure $ Left (OtherMsg, "The following files identified by LLM for modification do not exist: " <> T.intercalate ", " missing)

  filesToModifyLlmResult <- memoise (configCacheDir cfg) ("files_to_modify_for_refactor_" <> objectiveShortName) () (const "") $ \_ ->
    Engine.runAiFunc @bs filesToModifyCtxt HighIntelligenceRequired Engine.readOnlyTools exampleFilesToModify validateFilesToModify (configTaskMaxFailures cfg)

  let actualFilesToModify = relevantFileNames filesToModifyLlmResult
  if null actualFilesToModify
    then liftIO $ putTextLn "LLM indicated no existing files require modification."
    else liftIO $ putTextLn $ "LLM identified files to modify: " <> T.intercalate ", " actualFilesToModify

  -- 5. Iterate and Plan Refactor Tasks (per file to be modified)
  tasksForExistingFiles <- if null actualFilesToModify then pure [] else forM actualFilesToModify $ \fileNameToModify -> do
    liftIO $ putTextLn $ "Planning modifications for file: " <> fileNameToModify
    
    currentSt <- get
    let isCurrentlyFocused = Core.fileFocused fileNameToModify currentSt
    mOpenFile <- getOpenFile fileNameToModify <$> get

    unless isCurrentlyFocused $ do
        liftIO $ putTextLn $ "File " <> fileNameToModify <> " needs to be focused for planning modifications."
        unless (isJust mOpenFile) $ do
            liftIO $ putTextLn $ "File " <> fileNameToModify <> " was not open. Opening it now."
            Tools.openFile @bs Tools.DontFocusOpenedFile fileNameToModify cfg
        Tools.forceFocusFile fileNameToModify
    
    stAfterFocusCheck <- get
    unless (Core.fileFocused fileNameToModify stAfterFocusCheck) $
        liftIO $ putTextLn $ "Warning: File " <> fileNameToModify <> " may not be focused for planning. Max focused files: " <> show (cfg.configMaxNumFocusedFiles) <> ". Is file open and focusable?"
    
    let singleFileTaskCtxt =
          makeBaseContext background
            ( "Considering the overall objective (" <> objectiveShortName <> ": " <> task <> "),"
                <> " and now specifically planning modifications for file `" <> fileNameToModify <> "` (which is focused). "
                <> "What specific refactoring tasks are needed *for this file* (`" <> fileNameToModify <> "`)? "
                <> "Also, list any other files (dependencies) from the project that are directly relevant to performing these tasks on `" <> fileNameToModify <> "`; do not mention external dependencies."
                <> "Specify if unit tests for `" <> fileNameToModify <> "` should be updated as part of these changes (`refactorUpdateTests`)."
                <> "The 'refactorFile' field in your response MUST be '" <> fileNameToModify <> "'. "
                <> "Available source files for dependencies include: " <> T.intercalate ", " allSourceFileNames <> "."
                <> (if null refactorCfg.bigRefactorInitialOpenFiles
                    then ""
                    else " Also consider initial open files as potential dependencies: " <> T.intercalate ", " refactorCfg.bigRefactorInitialOpenFiles <> ".")
                <> (if null refactorCfg.bigRefactorSpecFiles
                    then ""
                    else " The following specification files are also open (unfocused): " <> T.intercalate ", " refactorCfg.bigRefactorSpecFiles <> ". If any spec file is crucial for understanding or implementing the task for `" <> fileNameToModify <> "`, or for verifying its correctness, please list it in `refactorFileDependencies`.")
            )
    let exampleDeps = L.nub $ 
          take 1 (filter (/= fileNameToModify) allSourceFileNames) ++
          take 1 (filter (/= fileNameToModify) refactorCfg.bigRefactorInitialOpenFiles) ++
          take 1 (filter (/= fileNameToModify) refactorCfg.bigRefactorSpecFiles)
    let exampleTargetedRefactorConfigItem = TargetedRefactorConfigItem
          { refactorFile = fileNameToModify
          , refactorTask = "Implement feature X in " <> fileNameToModify <> " by doing Y and Z."
          , refactorFileDependencies = exampleDeps
          , refactorUpdateTests = False -- LLM will decide this if bigRefactorDeliberatelyPromptForTestRefactors is true
          }

    let validateSingleItem :: Context -> TargetedRefactorConfigItem -> AppM (Either (MsgKind, Text) TargetedRefactorConfigItem)
        validateSingleItem _ item = do
          knownFileNames <- getAllKnownValidFileNames
          if item.refactorFile /= fileNameToModify
          then pure $ Left (OtherMsg, "Field 'refactorFile' in the response must be '" <> fileNameToModify <> "', but got '" <> item.refactorFile <> "'.")
          else do
            let missingDeps = filter (not . (`Set.member` knownFileNames)) item.refactorFileDependencies
            if null missingDeps
            then pure $ Right (if refactorCfg.bigRefactorDeliberatelyPromptForTestRefactors then item else item {refactorUpdateTests = False})
            else pure $ Left (OtherMsg, "The following dependencies for " <> fileNameToModify <> " do not exist: " <> T.intercalate ", " missingDeps)

    singleItem <- memoise (configCacheDir cfg) ("targeted_refactor_item_" <> objectiveShortName) fileNameToModify id $ \fileNameKey ->
        Engine.runAiFunc @bs singleFileTaskCtxt HighIntelligenceRequired Engine.readOnlyTools (exampleTargetedRefactorConfigItem { refactorFile = fileNameKey }) validateSingleItem (configTaskMaxFailures cfg)

    pure singleItem

  -- 6. Ask LLM for any new files that need to be created
  liftIO $ putTextLn "Asking LLM to identify any new files that need to be created..."
  let newFilesPrompt =
        makeBaseContext background
          ( "Given the overall objective (" <> objectiveShortName <> ": " <> task <> "),"
              <> " and the tasks already planned for existing files (listed below, this list might be empty if no existing files are modified): \n"
              <> (if null tasksForExistingFiles then "No tasks for existing files.\n" else Tools.toJ tasksForExistingFiles <> "\n")
              <> "Please identify if any *new files* need to be created. These could be for new modules, utilities, or to better organize code that doesn't fit well into existing files. If the task is to create the project from scratch, please think deeply about how the project should be structured before deciding on the file structure."
              <> "For each new file, provide its name, a summary of its purpose/content, and any dependencies it would have (on existing project files like "
              <> T.intercalate ", " allSourceFileNames
              <> (if null refactorCfg.bigRefactorInitialOpenFiles then "" else ", initially opened files like " <> T.intercalate ", " refactorCfg.bigRefactorInitialOpenFiles)
              <> (if null refactorCfg.bigRefactorSpecFiles
                  then ""
                  else ", or documentation/specification files like " <> T.intercalate ", " refactorCfg.bigRefactorSpecFiles)
              <> ", or other new files you are proposing in this same list). "
              <> "If a new file's purpose or content is dictated by a specific spec file, ensure that spec file is listed as a dependency. "
              <> "Please also include unit test files for every new file you create (although you may use one test file for multiple new files, where that fits better than one test per file). Tests should be positioned in the list right after the files they test."
              <> "If no new files are needed, return an empty list for 'items'.\n"
          )
  let exampleNewFileDeps = L.nub $
        take 1 allSourceFileNames ++ 
        take 1 (filter (`notElem` allSourceFileNames) refactorCfg.bigRefactorSpecFiles)
  let exampleNewFiles = ThingsWithDependencies
        { items = [ ThingWithDependencies "newAuthHandler.go" "Handles new authentication logic" exampleNewFileDeps
                  , ThingWithDependencies "newTypes.go" "Defines types for new authentication" []
                  ]
        }
  
  let validateNewFilesProposal :: Context -> ThingsWithDependencies -> AppM (Either (MsgKind, Text) [ThingWithDependencies])
      validateNewFilesProposal _ proposedNewThings@(ThingsWithDependencies newItems) = do
        currentSt <- get
        let diskAndSpecFileNames = Set.fromList $
              (map Core.existingFileName (stateFiles currentSt)) ++
              refactorCfg.bigRefactorSpecFiles

        let duplicateNames = filter (\item -> Set.member item.name diskAndSpecFileNames) newItems
        if not (null duplicateNames)
        then pure $ Left (OtherMsg, "Proposed new files conflict with already existing project files or spec files: " <> T.intercalate ", " (map (.name) duplicateNames))
        else do
          let existingFilesForDepCheck =
                Core.stateFiles currentSt ++
                (map (\sfName -> Core.ExistingFile sfName "") $
                 filter (\sfName -> sfName `notElem` map Core.existingFileName (Core.stateFiles currentSt)) refactorCfg.bigRefactorSpecFiles)

          case topologicalSortThingsWithDependencies existingFilesForDepCheck proposedNewThings of
            Left err -> pure $ Left (OtherMsg, "Error in new file dependencies or topology: " <> err)
            Right sortedNewItems -> pure $ Right sortedNewItems

  newFilesNeededList <- memoise (configCacheDir cfg) ("new_files_for_refactor_" <> objectiveShortName) () (const "") $ \_ ->
    Engine.runAiFunc @bs newFilesPrompt HighIntelligenceRequired Engine.readOnlyTools exampleNewFiles validateNewFilesProposal (configTaskMaxFailures cfg)

  -- 7. Convert new files to TargetedRefactorConfigItem tasks
  let newFileTasks = map (\twd -> TargetedRefactorConfigItem
        { refactorFile = twd.name
        , refactorTask = "NEW FILE: Create file '" <> twd.name <> "'. Purpose: " <> twd.summary
        , refactorFileDependencies = twd.dependencies
        , refactorUpdateTests = False -- Let LLM decide on tests for new files, or handle via explicit test creation tasks
        }) newFilesNeededList

  let allTasksRaw = tasksForExistingFiles ++ newFileTasks

  -- 8. Ask LLM to sort all tasks (existing file modifications + new file creations)
  unless (null allTasksRaw) $ liftIO $ putTextLn "Asking LLM to sort all refactoring tasks..."
  
  let sortTasksPrompt =
        makeBaseContext background
          ( "You are given a list tasks for project '" <> objectiveShortName <> "'. Some tasks involve modifying existing files, and some involve creating new files (indicated by 'NEW FILE:' in the task description). "
              <> "The overall objective is: " <> task <> ". \n"
              <> "Please sort these tasks in the order they should be performed. Key considerations for ordering: \n"
              <> "1. A file must be created before it can be modified or depended upon by another task. Tasks prefixed with 'NEW FILE:' are creation tasks. \n"
              <> "2. If task A modifies/creates file F1, and task B modifies/creates file F2 which lists F1 in its 'refactorFileDependencies', task A should generally precede task B. \n"
              <> "3. Consider the 'refactorFileDependencies' field for each task carefully. \n"
              <> "4. Specification files (" <> T.intercalate ", " refactorCfg.bigRefactorSpecFiles <> ") might be listed in dependencies; they are for reference and do not need to be 'created' by a task unless explicitly stated as a 'NEW FILE' task targeting a spec filename."
              <> "Return the sorted list of tasks in the same format as the input. Do not add, remove, or alter any tasks in terms of their content, only reorder them. Ensure all original tasks are present in the output.\n\n"
              <> "Tasks to sort:\n"
              <> Tools.toJ allTasksRaw
          )

  let validateSortedTasks :: [TargetedRefactorConfigItem] -> Context -> TargetedRefactorConfigItems -> AppM (Either (MsgKind, Text) [TargetedRefactorConfigItem])
      validateSortedTasks originalTasks _ (TargetedRefactorConfigItems sortedTasks) =
        if length originalTasks /= length sortedTasks
        then pure $ Left (OtherMsg, "Task sorting changed the number of tasks. Original: " <> show (length originalTasks) <> ", Sorted: " <> show (length sortedTasks) <> ". Please ensure all original tasks are returned.")
        else
          if Set.fromList (map refactorFile originalTasks) /= Set.fromList (map refactorFile sortedTasks)
          then pure $ Left (OtherMsg, "Task sorting seems to have dropped or added tasks based on 'refactorFile' names. Please verify against the original list.")
          else if Set.fromList originalTasks /= Set.fromList sortedTasks
          then pure $ Left (OtherMsg, "Task sorting modified tasks content or did not return all original tasks. The set of tasks must remain identical in content (refactorFile, refactorTask, etc.), only their order should change.")
          else pure $ Right sortedTasks
  
  sortedTaskItems <- if null allTasksRaw then pure [] else if not refactorCfg.bigRefactorDoFinalSort then pure allTasksRaw else
    memoise (configCacheDir cfg) ("sorted_tasks_for_refactor_" <> objectiveShortName) () (const "") $ \_ ->
      Engine.runAiFunc @bs sortTasksPrompt HighIntelligenceRequired Engine.readOnlyTools (TargetedRefactorConfigItems allTasksRaw) (validateSortedTasks allTasksRaw) (configTaskMaxFailures cfg)

  -- 9. Aggregate and Prepare Final Configuration
  liftIO $ putTextLn "Aggregating all planned and sorted tasks..."
  let finalTargetedRefactorConfig = TargetedRefactorConfig
        { refactorSummary = task
        , refactorFileTasks = sortedTaskItems
        , refactorOpenAllSourceFiles = True 
        }

  let planFileName = objectiveShortName <> "_targeted_refactor_plan.json"
  liftIO $ putTextLn $ "Writing final refactor plan to " <> planFileName
  writeFileIfDoesntExist planFileName (Tools.toJ finalTargetedRefactorConfig)

  -- 10. Execute the refactor project
  if null sortedTaskItems
  then liftIO $ putTextLn "No refactoring tasks identified or created. Nothing to execute."
  else do
    liftIO $ putTextLn "Executing the aggregated targeted refactor project..."
    makeTargetedRefactorProject @bs projectTexts finalTargetedRefactorConfig

  liftIO $ putTextLn "Targeted refactoring process complete."

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
      archRunner () = Engine.runAiFunc @bs archCtxt HighIntelligenceRequired Engine.readOnlyTools exampleArch Engine.validateAlwaysPass (configTaskMaxFailures cfg)
  plannedArch <- memoise (configCacheDir cfg) "architecture" () (const "") archRunner

  let ctxt = makeBaseContext (background <> "\n The architecture will be as follows: \n" <> plannedArch.description) makeFilenamesPrompt
      examplePlannedFiles =
        ThingsWithDependencies
          { items =
              [ ThingWithDependencies "someFile.go" "This file contains functionality for..." [],
                ThingWithDependencies "someOtherFile.go" "This file contains functionality for something different ..." ["someFile.go"]
              ]
          }
      runner () = Engine.runAiFunc @bs ctxt HighIntelligenceRequired Engine.readOnlyTools examplePlannedFiles validateFileNamesNoNestedPaths (configTaskMaxFailures cfg)
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

data TargetedRefactorConfigItems = TargetedRefactorConfigItems
 { refactorConfigItems :: [TargetedRefactorConfigItem]
 }
 deriving (Generic, Eq, Ord, Show)
instance ToJSON TargetedRefactorConfigItems
instance FromJSON TargetedRefactorConfigItems

data TargetedRefactorConfig = TargetedRefactorConfig
  { refactorSummary :: Text,
    refactorFileTasks :: [TargetedRefactorConfigItem],
    refactorOpenAllSourceFiles :: Bool
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
  _ <- Tools.buildAndTest @bs
  let filterSourceFile x = do
        buildable <- BS.isBuildableFile @bs x
        return $ buildable && not (T.isInfixOf "_test.go" x)

      setupOpenFiles fileNames = do
        modify' clearOpenFiles
        when refactorCfg.refactorOpenAllSourceFiles $ do
          st <- get
          sourceFileNames <- filterM filterSourceFile $ map existingFileName st.stateFiles
          forM_ sourceFileNames $ \x -> Tools.openFile @bs Tools.DontFocusOpenedFile x cfg
        forM_ (fileNames ++ [journalFileName]) $ \x -> Tools.openFile @bs Tools.DoFocusOpenedFile x cfg

      summary = refactorCfg.refactorSummary

      doRefactor :: TargetedRefactorConfigItem -> AppM ()
      doRefactor rCfg = do
        -- preSt <- get
        --unless (fileExists rCfg.refactorFile preSt) $ throwError $ "Trying to refactor file that doesn't exist: " <> rCfg.refactorFile <> "; files are: " <> show preSt.stateFiles
        let background = projectTexts.projectSummaryText <> "\n" <> summary
            exampleTasks =
              ThingsWithDependencies
                [ ThingWithDependencies "addNewClassX" "Class X, which does ..., must be added to support ..." [],
                  ThingWithDependencies "addNewFuncY" "Function Y, which does ..., must be added to support ..." ["addNewClassX"]
                ]
            relFiles = rCfg.refactorFile : rCfg.refactorFileDependencies
            autoRefactorUnitTests = if rCfg.refactorUpdateTests then DoAutoRefactorUnitTests else DontAutoRefactorUnitTests
            mkCtxt fileName =
              makeBaseContext background
                $ "Please return a list of tasks (if any; if the task is already done, the list may be empty) that must be done to modify/refactor "
                <> fileName
                <> " to achieve the objective:\n ------ \n"
                <> rCfg.refactorTask
                <> "\n ------ \n"
                <> "Each task should list other task dependencies if any, and there should be no circular dependencies (dependencies here means other tasks, not imports). Don't include tasks like running the build system and unit tests; they will run automatically after file changes.\n"
                <> "Please DO NOT attempt to call any tools to modify files to actually make the code changes, just Return a list of text values in the format specified below describing the individual tasks that need to be done.\n"
                <> "If there's something relevant for later that you can't encode well in the return value, please AppendFile=<[{\"fileName\": \"journal.txt\", \"rawTextName\": \"journalUpdateTextBoxName\"}]> it to the journal."
            taskBackground = background <> "\nYour task for this file is: " <> rCfg.refactorTask
            getChangesTask fileName = Engine.runAiFunc @bs (mkCtxt fileName) HighIntelligenceRequired (Tools.ToolAppendFile : Engine.readOnlyTools) exampleTasks validateThingsWithDependencies (configTaskMaxFailures cfg)
        setupOpenFiles relFiles
        Tools.forceFocusFile rCfg.refactorFile
        plannedTasks <- memoise (configCacheDir cfg) "file_tasks" rCfg.refactorFile id getChangesTask
        st <- get
        case getExistingFiles rCfg.refactorFileDependencies st of
          Left err -> throwError $ "Dependency listed for " <> rCfg.refactorFile <> " does not exist: " <> err
          Right depsFiles -> do
            sourceFileNames <- filterM filterSourceFile $ map existingFileName st.stateFiles
            let deps = map (\f -> (f, Tools.DoFocusOpenedFile)) depsFiles
                sourceFilesWithFocus = map (\x -> (ExistingFile x "", Tools.DontFocusOpenedFile)) sourceFileNames
                finalDeps = if refactorCfg.refactorOpenAllSourceFiles then deps ++ sourceFilesWithFocus else deps
            makeRefactorFileTask @bs taskBackground finalDeps rCfg.refactorFile plannedTasks autoRefactorUnitTests
  forM_ refactorCfg.refactorFileTasks doRefactor

data FileSpecComplianceAnalysisResult = FileSpecComplianceAnalysisResult
  { waysItDoesntMeetSpec :: Text,
    overallContentSummary :: Text
  }
  deriving (Generic, Eq, Ord, Show)

renderFileSpecComplianceAnalysisResult :: FileSpecComplianceAnalysisResult -> Text
renderFileSpecComplianceAnalysisResult (FileSpecComplianceAnalysisResult spec overall) = "FileSpecComplianceAnalysisResult{\n waysItDoesntMeetSpec: " <> spec <> "\n,\n overallContentSummary: " <> overall <> "\n}"

instance ToJSON FileSpecComplianceAnalysisResult
instance FromJSON FileSpecComplianceAnalysisResult

makeSpecComplianceAnalysisProject :: forall bs. (BS.BuildSystem bs) => ProjectTexts -> AppM ()
makeSpecComplianceAnalysisProject projectTexts = do
  cfg <- ask
  ignoredDirs <- BS.getIgnoredDirs @bs
  existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs cfg.configBaseDir
  modify' (updateExistingFiles existingFileNames)
  st <- get
  sourceFileNames <- filterM (BS.isBuildableFile @bs) $ map existingFileName st.stateFiles
  let background = projectTexts.projectSummaryText
      setupOpenFile fileName = do
        modify' clearOpenFiles
        forM_ sourceFileNames $ \x -> Tools.openFile @bs Tools.DontFocusOpenedFile x cfg
        Tools.openFile @bs Tools.DoFocusOpenedFile fileName cfg
      getSummary :: Text -> AppM (Text, FileSpecComplianceAnalysisResult)
      getSummary fileName = do
        let mkCtxt name =
              makeBaseContext background
                $ "Please check if "
                <> name
                <> " matches the specification, and return any ways it fails to satisfy it. Please also return detailed notes on its behaviour, for reference when checking other files. Double-check too that the logic there makes sense / has no bugs, and report anything that seems dubious/illogical."
            exampleRes = FileSpecComplianceAnalysisResult (fileName <> "doesn't meet the spec completely because it's supposed to ..., but it doesn't, and ...") "The file fulfills the following spec-relevant behaviours:"
            getChangesTask name = Engine.runAiFunc @bs (mkCtxt name) MediumIntelligenceRequired Engine.readOnlyTools exampleRes Engine.validateAlwaysPass (configTaskMaxFailures cfg)
        setupOpenFile fileName
        fileRes <- memoise (configCacheDir cfg) "file_analysis" fileName id getChangesTask
        return (fileName, fileRes)
  summaries <- forM sourceFileNames getSummary
  let summariesCat = T.unlines $ map (\(name, res) -> name <> ":\n" <> renderFileSpecComplianceAnalysisResult res) summaries
      combinedSummaryCtxt =
        makeBaseContext background
          $ "You previously analysed files in the project to identify any way they failed to match the spec. Now, based on the result of your analysis (which was per-file), can you see any other ways in which overall the project fails to match the spec? The analysis was:\n"
          <> summariesCat
      exampleCombinedRes = ThingWithDescription "Overall the project doesn't have any files that fulfil the ... requirement completely because ..."
      getCombinedSummary () = Engine.runAiFunc @bs combinedSummaryCtxt HighIntelligenceRequired Engine.readOnlyTools exampleCombinedRes Engine.validateAlwaysPass (configTaskMaxFailures cfg)
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
  Context ->
  -- | the proposed chunking plan
  SpecSegmentPlans ->
  AppM (Either (MsgKind, Text) SpecSegmentPlans)
validateSpecSegmentPlans docLength _ ssp@(SpecSegmentPlans segments) = do
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
      -- you can add `Tools.ToolAppendFile` to Engine.readOnlyTools:
      chunkingTools = Tools.ToolAppendFile : Engine.readOnlyTools

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
  forM_ segmentPlansResult.segmentPlans $ \SpecSegmentPlan {..} -> do
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
              <> Tools.toJ segmentPlansResult.segmentPlans
          )
          archPrompt
      exampleArch =
        ThingWithDescription
          "Overall architecture referencing sub-spec doc files..."
      archRunner () =
        Engine.runAiFunc @bs
          archCtxt
          HighIntelligenceRequired
          Engine.readOnlyTools
          exampleArch
          Engine.validateAlwaysPass
          (configTaskMaxFailures cfg)

  plannedArch <- memoise (configCacheDir cfg) "architecture" () (const "") archRunner

  -- 6) Code file planning
  let background =
        projectTexts.projectSummaryText
          <> "\nThe architecture will be as follows:\n"
          <> plannedArch.description
          <> "\nWe have these doc files for reference:\n"
          <> Tools.toJ segmentPlansResult.segmentPlans

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
          Engine.readOnlyTools
          examplePlannedFiles
          validateFileNamesNoNestedPaths
          (configTaskMaxFailures cfg)

  plannedFiles <- memoise (configCacheDir cfg) "file_planner" () (const "") runner

  writeFileIfDoesntExist "architecture.txt" plannedArch.description
  writeFileIfDoesntExist "files_summary.json" (Tools.toJ plannedFiles)
  writeFileIfDoesntExist "docs_summary.json" (Tools.toJ segmentPlansResult)

  -- 7) Create each file
  forM_ plannedFiles (makeFile @bs filePlanCtxt.contextBackground [])

  putTextLn "Finished creating project based on spec!"


data SimpleResponse = SimpleResponse
  { response :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON SimpleResponse
instance FromJSON SimpleResponse

-- | An interactive workflow that reads user input, sends it to the LLM,
--   and prints the response, looping indefinitely.
--   Includes project summary in context and opens all source files unfocused.
makePromptResponseProject :: forall bs. (BS.BuildSystem bs) => ProjectTexts -> AppM ()
makePromptResponseProject projectTexts = do
    cfg <- ask

    ignoredDirs <- BS.getIgnoredDirs @bs
    existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs cfg.configBaseDir
    modify' (updateExistingFiles existingFileNames)
    _ <- Tools.buildAndTest @bs
    st <- get

    -- Define the filter function
    let filterSourceFile x = do
            buildable <- BS.isBuildableFile @bs x
            return $ buildable && not (T.isInfixOf "_test.go" x)

    sourceFileNames <- filterM filterSourceFile $ map existingFileName st.stateFiles

    -- Open all source files without focusing
    liftIO $ putTextLn "Opening source files (unfocused)..."
    modify' clearOpenFiles 
    forM_ sourceFileNames $ \fileName -> do
        Tools.openFile @bs Tools.DontFocusOpenedFile fileName cfg
    liftIO $ putTextLn $ "Opened " <> show (length sourceFileNames) <> " source files."

    -- Prepare the context background including project summary
    let baseBackground = projectTexts.projectSummaryText
        --assistantRole = "You are a helpful AI assistant aware of the project context (source files listed above). Please respond directly and concisely to the user's input below."
        -- Combine project summary and assistant role for the final background
        --background = baseBackground <> "\n\n" <> assistantRole
        background = baseBackground

        tools = Engine.allTools
        -- Example response for the LLM call structure.
        exampleResp = SimpleResponse { response = "This is an example response text." }

    liftIO $ putTextLn "Starting interactive LLM mode."
    liftIO $ putTextLn "Enter your text prompt below (end input with an empty line)."
    liftIO $ putTextLn "Press Ctrl+C to exit the application."

    contextRef <- liftIO $ newIORef Nothing

    -- Loop indefinitely until user interruption (e.g., Ctrl+C)
    forever $ do
        -- Read potentially multi-line input from the user
        userLines <- liftIO TerminalInput.readLinesUntilEmpty

        -- Only proceed if the user entered some text
        unless (null userLines) $ do

            mCurrentCtxt <- liftIO $ readIORef contextRef
            let userTask = T.unlines userLines
                -- Use the pre-computed background
                ctxtForThisTurn = case mCurrentCtxt of
                  Just existingCtxt -> addToContextUser existingCtxt OtherMsg userTask
                  Nothing -> makeBaseContext background userTask

            -- No validation needed beyond successful JSON parsing, hence Engine.validateAlwaysPass.
            (llmResult, ctxtAfter) <- Engine.runAiFuncKeepingContext @bs
                           ctxtForThisTurn
                           MediumIntelligenceRequired -- Adjust intelligence as needed
                           tools
                           exampleResp
                           Engine.validateAlwaysPass
                           (configTaskMaxFailures cfg)

            -- Print the LLM's response
            liftIO $ putTextLn $ "\nLLM Response:\n" <> llmResult.response
            liftIO $ putTextLn "" -- Add spacing before the next prompt
            liftIO $ writeIORef contextRef (Just ctxtAfter)

-- | Data type for the LLM to return a list of irrelevant files to close.
data IrrelevantFilesToClose = IrrelevantFilesToClose
  { filesToClose :: [Text]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON IrrelevantFilesToClose
instance FromJSON IrrelevantFilesToClose

-- | Validates the LLM's response for irrelevant files.
-- Checks if the suggested files are currently open.
validateIrrelevantFilesToClose :: Context -> IrrelevantFilesToClose -> AppM (Either (MsgKind, Text) IrrelevantFilesToClose)
validateIrrelevantFilesToClose _ returnedData@(IrrelevantFilesToClose toClose) = do
  st <- get
  let openFileNames = Set.fromList $ map Core.openFileName (stateOpenFiles st)
  let notCurrentlyOpen = filter (\fName -> not (Set.member fName openFileNames)) toClose
  
  if null notCurrentlyOpen
    then pure $ Right returnedData
    else pure $ Left (OtherMsg, "LLM suggested closing files that are not currently open: " <> T.intercalate ", " notCurrentlyOpen)

-- | Helper function to identify and close irrelevant unfocused files.
-- Takes a background text, a list of changes, and the primary filename being modified.
-- Queries the LLM to find unfocused files irrelevant to the task, then closes them.
closeIrrelevantUnfocusedFiles :: forall bs. (BS.BuildSystem bs) 
                              => Text -- ^ General background for the LLM.
                              -> [ThingWithDependencies] -- ^ List of changes defining the current task.
                              -> Text -- ^ The primary filename being modified for the task.
                              -> AppM ()
closeIrrelevantUnfocusedFiles llmBackground taskChanges mainFileName = do
  cfg <- ask
  st <- get

  let unfocusedOpenFileObjs = filter (not . Core.openFileFocused) (stateOpenFiles st)
      unfocusedOpenFileNamesInitial = map Core.openFileName unfocusedOpenFileObjs
      unfocusedOpenFileNames = filter (\x -> not $ T.isInfixOf ".txt" x) unfocusedOpenFileNamesInitial

  if null unfocusedOpenFileNames then do
    liftIO $ putTextLn "No unfocused files are open. Skipping LLM call to identify irrelevant files."
    pure ()
  else do
    liftIO $ putTextLn $ "Currently unfocused open files: " <> T.intercalate ", " unfocusedOpenFileNames

    let taskSummary = "Your task is to close open source files (not documentation/spec files) that are completely irrelevant for the main task; the main task (which will be done after the irrelevant files are closed) is:\nTo modify the file '" <> mainFileName <> "' based on the following required changes: " <> Tools.toJ taskChanges <> "."
    
    let llmSpecificTaskPrompt =
          taskSummary <> "\n\n" <>
          "You can see the full list of currently open files and their focus status. " <>
          "From the files that are currently open but *not* focused (which are explicitly: " <> T.intercalate ", " unfocusedOpenFileNames <> "), " <>
          "please identify any of these unfocused files that provide no relevant/useful information/type/function definitions for achieving the main task described above." <>
          "These irrelevant files can be closed to reduce clutter in the context and improve focus. " <>
          "Be careful not to close any core types files, e.g. types.go, common.go, config.go, utils.go or the like that are likely to be widely used. Never close main.go!" <>
          "Note that unfocused source files only show function types and datatypes, not function bodies. " <>
          "Return a JSON object with a single key 'filesToClose' containing a list of just the filenames (from the provided unfocused list) that you determine are completely irrelevant; don't try to take any tool actions to actually close the files. " <>
          "If all unfocused files have some relevance, or if you are unsure, return an empty list for 'filesToClose'. Err on the side of caution; if there's even a tiny chance a file might be useful, don't close it!" <>
          "DO NOT attempt to make any file changes or call anytools to do so, just return a list in the format below."

    let aiContext = makeBaseContext llmBackground llmSpecificTaskPrompt 
    
    let exampleReturn = IrrelevantFilesToClose { filesToClose = take (min 1 (length unfocusedOpenFileNames)) unfocusedOpenFileNames }
    
    let toolsForLlm = Engine.readOnlyToolsNoFileActions -- LLM should only identify, not perform actions

    liftIO $ putTextLn "Querying LLM to identify irrelevant unfocused files to close..."

    -- Mark it nested so it won't attempt to go into error fixing mode etc.
    let getIrrelevantFiles () = fst <$> Engine.runAiFuncInner @bs Engine.IsNestedAiFuncTrue Engine.IsCloseFileTaskFalse
          aiContext
          MediumIntelligenceRequired -- This task should generally not require high intelligence.
          toolsForLlm
          exampleReturn
          validateIrrelevantFilesToClose
          (configTaskMaxFailures cfg)
    irrelevantFilesResult <- memoise (configCacheDir cfg) ("files_to_close_for_" <> mainFileName) () (const "") getIrrelevantFiles
    let filesIdentifiedByLlm = filesToClose irrelevantFilesResult

    if null filesIdentifiedByLlm
    then liftIO $ putTextLn "LLM identified no unfocused files as completely irrelevant."
    else do
      liftIO $ putTextLn $ "LLM suggested the following unfocused files might be irrelevant: " <> T.intercalate ", " filesIdentifiedByLlm
      
      -- Before closing, re-check current state for safety.
      currentState <- get
      let currentlyOpenFileObjects = stateOpenFiles currentState
      
      let filesToActuallyClose = filter (\fName -> 
            -- Check if the file is still in the list of open files
            case find (\opf -> Core.openFileName opf == fName) currentlyOpenFileObjects of
              Just openFile -> not (Core.openFileFocused openFile) -- And ensure it's still unfocused
              Nothing       -> False -- File is no longer open
            ) filesIdentifiedByLlm

      if null filesToActuallyClose
      then liftIO $ putTextLn "After re-checking, no valid (still open and unfocused) irrelevant files to close. They might have been closed or focused by other operations."
      else do
        liftIO $ putTextLn $ "Proceeding to close the following confirmed irrelevant and unfocused files: " <> T.intercalate ", " filesToActuallyClose
        forM_ filesToActuallyClose $ \fName -> do
          liftIO $ putTextLn $ "Closing file: " <> fName
          modify' (Core.closeOpenFile fName) 
          -- Note: For full event tracing, a `Tools.closeFile` that logs EvtCloseFile could be used here.
        liftIO $ putTextLn $ "Successfully closed " <> T.pack (show $ length filesToActuallyClose) <> " irrelevant unfocused files."

