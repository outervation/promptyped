{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import CPlusPlus (CPlusPlusLang)
import Core
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Text qualified as T
import FileSystem qualified as FS
import GoLang (GoLang)
import Logging qualified
import PromptCommon
import Relude
import System.FilePath qualified as FP
import System.Log.Logger qualified as Logger

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
    taskMaxFailures :: Int
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON AppConfig

instance ToJSON AppConfig

loadConfig :: FilePath -> IO (Either Text AppConfig)
loadConfig path = do
  contents <- BS.readFile path
  return $ (bimap T.pack id) $ eitherDecode contents

makeGoBinanceApiDataRecorder :: AppConfig -> IO ()
makeGoBinanceApiDataRecorder aCfg = do
  let cannotModifyDepReason = "You should not need to import any extra external libraries for this project, the stdlib can do everything you need."
  let cfg =
        Config
          { configApiKey = apiKey aCfg,
            configApiSite = apiSite aCfg,
            configModel = modelName aCfg,
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
              ]
          }
  existingFileNames <- FS.getFileNamesRecursive ["build", "contrib", ".git"] (T.unpack $ baseDir aCfg)
  let existingFiles = map (`ExistingFile` "") existingFileNames
  let initialState = AppState mempty [] existingFiles (CompileTestState Nothing Nothing)
  let logDir = T.unpack $ logFileDir aCfg
  let logPath = logDir FP.</> "promptyped_binapi_downloader.log"
  let debugLogPath = logDir FP.</> "promptyped_binapi_downloader.debug.log"
  Logging.initializeLogger logPath debugLogPath Logger.INFO
  liftIO $ Logging.logInfo "Initial config" (show cfg)
  liftIO $ Logging.logInfo "Initial state" (show initialState)
  res <- runApp cfg initialState (makeProject @GoLang)
  putTextLn $ "Result: " <> show res

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cfgPath] -> do
      cfgE <- loadConfig cfgPath
      case cfgE of
        Right cfg ->
          makeGoBinanceApiDataRecorder cfg
        Left err -> do
          putTextLn $ "Error loading config: " <> err
          exitFailure
    _ -> do
      putTextLn "Usage: program <config-path>"
      exitFailure
