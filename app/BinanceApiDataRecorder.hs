{-# LANGUAGE OverloadedStrings #-}

module BinanceApiDataRecorder where

import AppConfig
import Core
import Data.Text qualified as T
import GoLang (GoLang)
import Logging qualified
import PromptCommon
import Relude
import System.FilePath qualified as FP
import System.Log.Logger qualified as Logger

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
  let initialState = AppState mempty [] [] (CompileTestState Nothing Nothing)
  let logDir = T.unpack $ logFileDir aCfg
  let logPath = logDir FP.</> "promptyped_binapi_downloader.log"
  let debugLogPath = logDir FP.</> "promptyped_binapi_downloader.debug.log"
  Logging.initializeLogger logPath debugLogPath Logger.INFO
  liftIO $ Logging.logInfo "Initial config" (show cfg)
  liftIO $ Logging.logInfo "Initial state" (show initialState)
  res <- runApp cfg initialState (makeProject @GoLang)
  putTextLn $ "Result: " <> show res
