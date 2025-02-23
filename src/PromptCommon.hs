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
import Logging qualified
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

topologicalSortThingsWithDependencies :: [ExistingFile] -> ThingsWithDependencies -> Either Text [ThingWithDependencies]
topologicalSortThingsWithDependencies existingFiles (ThingsWithDependencies files) = do
  let isDoc name = isDocFileExtension name && elem name (map existingFileName existingFiles)
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
allTools = [Tools.ToolOpenFile, Tools.ToolCloseFile, Tools.ToolAppendFile, Tools.ToolInsertInFile, Tools.ToolEditFile, Tools.ToolRevertFile, Tools.ToolPanic, Tools.ToolReturn]

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
  let runner fileName' = Engine.runAiFunc @bs (makeCtxt (makeUnitTestsPrompt fileName')) allTools exampleUnitTests validateUnitTests (configTaskMaxFailures cfg)
  planRes <- memoise (configCacheDir cfg) "test_planner" fileName show runner
  let testFileName = unitTestFileName planRes
  Tools.openFile testFileName cfg
  let exampleUnitTestDone = UnitTestDone True
  let makeUnitTest UnitTest {..} = Engine.runAiFunc @bs (makeCtxt $ makeUnitTestPrompt fileName testFileName unitTestName unitTestSummary) allTools exampleUnitTestDone validateUnitTest (configTaskMaxFailures cfg)
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
    let runner fileName = Engine.runAiFunc @bs (makeCtxt fileName) allTools exampleCreatedFiles validateCreatedFiles (configTaskMaxFailures cfg)
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
      archRunner () = Engine.runAiFunc @bs archCtxt tools exampleArch validateAlwaysPass (configTaskMaxFailures cfg)
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
      runner () = Engine.runAiFunc @bs ctxt tools examplePlannedFiles validateThingsWithDependencies (configTaskMaxFailures cfg)
  plannedFiles <- memoise (configCacheDir cfg) "file_planner" () (const "") runner
  forM_ plannedFiles (makeFile @bs ctxt.contextBackground)
  finalState <- get
  liftIO $ Logging.logInfo "Final config" (show cfg)
  liftIO $ Logging.logInfo "Final state" (show $ stateMetrics finalState)
  liftIO . putTextLn . show $ cfg
  liftIO . putTextLn . show $ stateMetrics finalState
