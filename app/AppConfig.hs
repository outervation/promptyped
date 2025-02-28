module AppConfig where

import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Text qualified as T
import Relude

data ProjectKind = CreateProject | RefactorProject
  deriving (Generic, Eq, Ord, Show)

instance FromJSON ProjectKind

instance ToJSON ProjectKind

data AppConfig = AppConfig
  { apiKey :: Text,
    apiSite :: Text,
    modelName :: Text,
    baseDir :: Text,
    cacheDir :: Text,
    logFileDir :: Text,
    buildTimeoutSeconds :: Int,
    buildNumJobs :: Int,
    gitUserName :: Text,
    gitUserEmail :: Text,
    taskMaxFailures :: Int,
    projectKind :: ProjectKind
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON AppConfig

instance ToJSON AppConfig

loadConfig :: FilePath -> IO (Either Text AppConfig)
loadConfig path = do
  contents <- BS.readFile path
  return $ (bimap T.pack id) $ eitherDecode contents
