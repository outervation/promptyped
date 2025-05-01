module Logging (logInfo, logWarn, logError, logDebug, initializeLogger) where

import Data.Text as T
import Relude
import System.Directory qualified as DIR
import System.FilePath qualified as FP
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

initializeLogger :: FilePath -> FilePath -> Priority -> IO ()
initializeLogger mainLogFile debugLogFile priority = do
  removeAllHandlers

  DIR.createDirectoryIfMissing True (FP.takeDirectory mainLogFile)
  DIR.createDirectoryIfMissing True (FP.takeDirectory debugLogFile)

  -- Create main file handler for non-debug logs
  mainHandler <- fileHandler mainLogFile priority

  -- Create debug file handler that only accepts debug messages
  debugHandler <- fileHandler debugLogFile DEBUG

  -- Create a formatter that includes timestamp
  let format = simpleLogFormatter "[$time $loggername $prio] $msg"

  -- Set formatters for both handlers
  let mainHandlerFormatted = setFormatter mainHandler format
      debugHandlerFormatted = setFormatter debugHandler format

  -- Update the root logger with both handlers
  updateGlobalLogger rootLoggerName (setLevel (min priority DEBUG)) -- Ensure DEBUG messages are processed
  updateGlobalLogger rootLoggerName (addHandler mainHandlerFormatted)
  updateGlobalLogger rootLoggerName (addHandler debugHandlerFormatted)

logInfo :: Text -> Text -> IO ()
logInfo context msg = do
  infoM (T.unpack context) (T.unpack msg)

logWarn :: Text -> Text -> IO ()
logWarn context msg = do
  warningM (T.unpack context) (T.unpack msg)

logError :: Text -> Text -> IO ()
logError context msg = do
  errorM (T.unpack context) (T.unpack msg)

logDebug :: Text -> Text -> IO ()
logDebug context msg = do
  debugM (T.unpack context) (T.unpack msg)
