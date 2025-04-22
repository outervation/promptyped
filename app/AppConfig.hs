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

data ModelConfig = ModelConfig
  { apiKey :: Text,
    apiSite :: Text,
    lowIntModelName :: Text,
    mediumIntModelName :: Text,
    highIntModelName :: Text,
    taskMaxFailures :: Int,
    rejectInvalidSyntaxDiffs :: Bool,
    maxNumFocusedFiles :: Int,
    modelTemperature :: Maybe Float,
    modelMaxInputTokens :: Int
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON ModelConfig

instance ToJSON ModelConfig

data TaskConfig = TaskConfig
  { projectName :: Text,
    specFilePath :: Text,
    dependencies :: [Text]
    --    initialFiles :: [Text]
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON TaskConfig

instance ToJSON TaskConfig

data AppConfig = AppConfig
  { baseDir :: Text,
    cacheDir :: Text,
    logFileDir :: Text,
    buildTimeoutSeconds :: Int,
    buildNumJobs :: Int,
    gitUserName :: Text,
    gitUserEmail :: Text,
    envVars :: [(Text, Text)],
    projectKind :: ProjectKind,
    targetedRefactorCfg :: Maybe PC.TargetedRefactorConfig,
    bigRefactorCfg :: Maybe PC.BigRefactorConfig,
    taskCfg :: Maybe TaskConfig
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON AppConfig

instance ToJSON AppConfig

appAndModelConfigToConfig :: AppConfig -> ModelConfig -> Config
appAndModelConfigToConfig aCfg mCfg =
  let cannotModifyDepReason = "You should not need to import any extra external libraries for this project, the stdlib can do everything you need."
   in Config
        { configApiKey = apiKey mCfg,
          configApiSite = apiSite mCfg,
          configLowIntModel = lowIntModelName mCfg,
          configMediumIntModel = mediumIntModelName mCfg,
          configHighIntModel = highIntModelName mCfg,
          configBaseDir = T.unpack $ baseDir aCfg,
          configCacheDir = T.unpack $ cacheDir aCfg,
          configBuildTimeoutSeconds = buildTimeoutSeconds aCfg,
          configBuildNumJobs = buildNumJobs aCfg,
          configGitUserName = gitUserName aCfg,
          configGitUserEmail = gitUserEmail aCfg,
          configEnvVars = envVars aCfg,
          configMaxNumFocusedFiles = maxNumFocusedFiles mCfg,
          configTaskMaxFailures = RemainingFailureTolerance (taskMaxFailures mCfg),
          configRejectInvalidSyntaxDiffs = rejectInvalidSyntaxDiffs mCfg,
          configForbiddenFiles =
            [ ForbiddenFile "go.mod" cannotModifyDepReason,
              ForbiddenFile "go.sum" cannotModifyDepReason
            ],
          configModelTemperature = modelTemperature mCfg,
          configModelMaxInputTokens = modelMaxInputTokens mCfg
        }

loadConfig :: FilePath -> FilePath -> IO (Either Text (AppConfig, ModelConfig))
loadConfig appCfgPath modelCfgPath = do
  appBytes <- BS.readFile appCfgPath
  modelBytes <- BS.readFile modelCfgPath
  let appE = first T.pack (eitherDecode appBytes) :: Either T.Text AppConfig
      modelE = first T.pack (eitherDecode modelBytes) :: Either T.Text ModelConfig
  pure $ liftA2 (,) appE modelE
