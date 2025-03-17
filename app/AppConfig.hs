{-# LANGUAGE OverloadedStrings #-}

module AppConfig where

import Core
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Text qualified as T
import PromptCommon qualified as PC
import Relude

data ProjectKind = CreateProject | RefactorProject | TargetedRefactorProject | FileAnalysisProject
  deriving (Generic, Eq, Ord, Show)

instance FromJSON ProjectKind

instance ToJSON ProjectKind

data AppConfig = AppConfig
  { apiKey :: Text,
    apiSite :: Text,
    lowIntModelName :: Text,
    mediumIntModelName :: Text,
    highIntModelName :: Text,
    baseDir :: Text,
    cacheDir :: Text,
    logFileDir :: Text,
    buildTimeoutSeconds :: Int,
    buildNumJobs :: Int,
    gitUserName :: Text,
    gitUserEmail :: Text,
    taskMaxFailures :: Int,
    projectKind :: ProjectKind,
    modelTemperature :: Maybe Float,
    modelMaxInputTokens :: Int,
    targetedRefactorCfg :: Maybe PC.TargetedRefactorConfig,
    bigRefactorCfg :: Maybe PC.BigRefactorConfig
  }
  deriving (Generic, Eq, Ord, Show)

appConfigToConfig :: AppConfig -> Config
appConfigToConfig aCfg =
  let cannotModifyDepReason = "You should not need to import any extra external libraries for this project, the stdlib can do everything you need."
   in Config
        { configApiKey = apiKey aCfg,
          configApiSite = apiSite aCfg,
          configLowIntModel = lowIntModelName aCfg,
          configMediumIntModel = mediumIntModelName aCfg,
          configHighIntModel = highIntModelName aCfg,
          configBaseDir = T.unpack $ baseDir aCfg,
          configCacheDir = T.unpack $ cacheDir aCfg,
          configBuildTimeoutSeconds = buildTimeoutSeconds aCfg,
          configBuildNumJobs = buildNumJobs aCfg,
          configGitUserName = gitUserName aCfg,
          configGitUserEmail = gitUserEmail aCfg,
          configEnvVars = [],
          configTaskMaxFailures = RemainingFailureTolerance (taskMaxFailures aCfg),
          configForbiddenFiles =
            [ ForbiddenFile "go.mod" cannotModifyDepReason,
              ForbiddenFile "go.sum" cannotModifyDepReason
            ],
          configModelTemperature = modelTemperature aCfg,
          configModelMaxInputTokens = modelMaxInputTokens aCfg
        }

instance FromJSON AppConfig

instance ToJSON AppConfig

loadConfig :: FilePath -> IO (Either Text AppConfig)
loadConfig path = do
  contents <- BS.readFile path
  return $ (bimap T.pack id) $ eitherDecode contents
