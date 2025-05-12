{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TaskFromConfig where

import AppConfig
import BuildSystem (BuildSystem)
import CPlusPlus qualified as CPlusPlusMod
import Control.Exception (ErrorCall (..), throwIO)
import Control.Monad.Except
import Core
import Data.Aeson
import Data.Text qualified as T
import FileSystem qualified as FS
import GoLang qualified as GoLangMod
import Logging qualified
import PromptCommon
import Relude
import System.FilePath qualified as FP
import System.Log.Logger qualified as Logger

data SomeBuildSystem where
  SomeBuildSystem :: forall bs. (BuildSystem bs) => Proxy bs -> SomeBuildSystem

data ProgLangName = CPlusPlus | GoLang
  deriving (Generic, Eq, Ord, Show)

instance ToJSON ProgLangName

instance FromJSON ProgLangName

getBuildSystem :: ProgLangName -> SomeBuildSystem
getBuildSystem CPlusPlus = SomeBuildSystem (Proxy @CPlusPlusMod.CPlusPlusLang)
getBuildSystem GoLang = SomeBuildSystem (Proxy @GoLangMod.GoLang)

withBuildSystem ::
  SomeBuildSystem ->
  (forall bs. (BuildSystem bs) => Proxy bs -> r) -> -- k MUST take Proxy bs
  r
withBuildSystem (SomeBuildSystem (p :: Proxy bs)) k = k p

-- 1. Match binds 'p' (value) and 'bs' (type, via ScopedTypeVariables)
-- 2. Guarantees BuildSystem bs constraint holds for 'bs'
-- 3. Calls the continuation 'k' with the proxy 'p'

getTaskCfg :: AppConfig -> IO TaskConfig
getTaskCfg aCfg = do
  let cfg = aCfg.taskCfg
  case cfg of
    Just x -> return x
    Nothing -> throwIO $ ErrorCall "Missing taskCfg for TaskFromConfig"

makeTaskFromConfig :: AppConfig -> ModelConfig -> IO ()
makeTaskFromConfig aCfg mCfg = do
  let cfg = appAndModelConfigToConfig aCfg mCfg
  tCfg <- getTaskCfg aCfg
  spec <- FS.readFileToTextMayThrow (T.unpack tCfg.specFilePath)
  let projectCfg =
        ProjectConfig
          { projectDependencyNames = tCfg.dependencies,
            projectInitialFiles = [] -- tCfg.initialFiles
          }
      backgroundTexts = spec
      projectTexts = ProjectTexts {projectSummaryText = backgroundTexts}
      initialState = AppState mempty [] [] mempty
      logDir = T.unpack $ logFileDir aCfg
      projectNameStr = T.unpack tCfg.projectName
      logPath = logDir FP.</> projectNameStr <> ".log"
      debugLogPath = logDir FP.</> projectNameStr <> ".debug.log"
  Logging.initializeLogger logPath debugLogPath Logger.INFO
  liftIO $ Logging.logInfo "Initial config" (show cfg)
  liftIO $ Logging.logInfo "Initial state" (show initialState)
  let projectFn :: AppM ()
      projectFn = case projectKind aCfg of
        RefactorProject -> case bigRefactorCfg aCfg of
          Just refactorCfg -> withBuildSystem (getBuildSystem GoLang) $ \(_ :: Proxy bs) -> makeRefactorFilesProject @bs projectTexts refactorCfg
          Nothing -> throwError "Missing big refactor config!"
        TargetedRefactorProject -> case targetedRefactorCfg aCfg of
          Just refactorCfg -> withBuildSystem (getBuildSystem GoLang) $ \(_ :: Proxy bs) -> makeTargetedRefactorProject @bs projectTexts refactorCfg
          Nothing -> throwError "Missing targeted refactor config!"
        ChatProject -> withBuildSystem (getBuildSystem GoLang) $ \(_ :: Proxy bs) -> makePromptResponseProject @bs projectTexts
        FileAnalysisProject -> withBuildSystem (getBuildSystem GoLang) $ \(_ :: Proxy bs) -> makeFileAnalysisProject @bs projectTexts
        _ -> throwError $ "Unsupported project kind for TaskFromConfig: " <> show (projectKind aCfg)
  res <- runApp cfg initialState projectFn
  case res of
    Left err -> putTextLn $ "Process ended with error: " <> show err
    Right ((), finalState) -> do
      liftIO $ Logging.logInfo "Final config" (show cfg)
      liftIO $ Logging.logInfo "Final state" (show $ stateMetrics finalState)
      liftIO . putTextLn . show $ cfg
      liftIO . putTextLn . show $ stateMetrics finalState
