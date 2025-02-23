module Logging (logInfo, logWarn, initializeLogger) where

import Data.Text as T
import Relude
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

initializeLogger :: FilePath -> Priority -> IO ()
initializeLogger logFile priority = do
  removeAllHandlers
  -- Create a file handler
  theFileHandler <- fileHandler logFile priority

  -- Create a formatter that includes timestamp
  let format = simpleLogFormatter "[$time $loggername $prio] $msg"

  -- Set the formatter for the handler
  let handler = setFormatter theFileHandler format

  -- Update the root logger
  updateGlobalLogger rootLoggerName (setLevel priority)
  updateGlobalLogger rootLoggerName (addHandler handler)

logInfo :: Text -> Text -> IO ()
logInfo context msg = do
  infoM (T.unpack context) (T.unpack msg)

logWarn :: Text -> Text -> IO ()
logWarn context msg = do
  warningM (T.unpack context) (T.unpack msg)
