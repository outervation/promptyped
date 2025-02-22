{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import CPlusPlus (CPlusPlusLang)
import Core
-- import Data.Graph.StronglyConnComp (SCC(..), stronglyConnComp)
import Data.Text qualified as T
import FileSystem qualified as FS
import GoLang (GoLang)
import Logging qualified
import PromptCommon
import Relude
import System.Log.Logger qualified as Logger

-- Inputs, Available Actions, Final Outputs. All named, and typed.
-- Stop on final output.
-- Transparent handlers for certain actions

testIt2 :: IO ()
testIt2 = do
  let testMessages = [(OtherMsg, Message "user" "hello there"), (OtherMsg, Message "assistant" "a very long first response that should get truncated"), (OtherMsg, Message "user" "thanks"), (OtherMsg, Message "assistant" "a second long response that should get truncated too"), (OtherMsg, Message "user" "cool"), (OtherMsg, Message "assistant" "third response - should stay full"), (OtherMsg, Message "assistant" "fourth response - should also stay full")]
  let result = truncateOldMessages "assistant" 2 10 testMessages
  mapM_ (\(_, msg) -> putTextLn $ msg.role <> ": " <> msg.content) result

testIt :: Text -> Text -> Text -> IO ()
testIt aiApiSite aiApiKey model = do
  let baseDir = "/home/dfxs/gobinapi_o3"
  let cannotModifyDepReason = "You should not need to import any extra external libraries for this project, the stdlib can do everything you need."
  let cfg =
        Config
          { configApiKey = aiApiKey,
            configApiSite = aiApiSite,
            configModel = model,
            configBaseDir = baseDir,
            configCacheDir = "/home/dfxs/memoisationDir_03",
            configBuildTimeoutSeconds = 1000,
            configBuildNumJobs = 32,
            configEnvVars = [],
            configTaskMaxFailures = RemainingFailureTolerance 10,
            configForbiddenFiles =
              [ ForbiddenFile "go.mod" cannotModifyDepReason,
                ForbiddenFile "go.sum" cannotModifyDepReason
              ]
          }
  existingFileNames <- FS.getFileNamesRecursive ["build", "contrib", ".git"] baseDir
  let existingFiles = map (`ExistingFile` "") existingFileNames
  let initialState = AppState mempty [] existingFiles (CompileTestState Nothing Nothing)
  Logging.initializeLogger "/home/dfxs/log.txt" Logger.INFO
  res <- runApp cfg initialState (makeProject @GoLang)
  putTextLn $ "Result: " <> show res

{- First result o3-mini-high
Metrics {metricsTokensIn = 3019042, metricsTokensOut = 1699719, metricsCost = 9.858881999999996, metricsApiTime = 15758381099322}
-}

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  args <- getArgs
  case args of
    [apiSite, apiKey, model] -> do
      let keyT = T.pack apiKey
          siteT = T.pack apiSite
          modelT = T.pack model
      testIt siteT keyT modelT
    _ -> putTextLn "Usage: program <ml-api-name> <ml-api-key> <model>"

tmpp :: Text
tmpp = "return=[<{\n  \"plannedFiles\": [\n    {\n      \"plannedFileDependencies\": [],\n      \"plannedFileName\": \"config.h\",\n      \"plannedFileSummary\": \"Defines configuration structures and handles JSON deserialization using boost::describe.\"\n    },\n    {\n      \"plannedFileDependencies\": [\"config.h\"],\n      \"plannedFileName\": \"json_serialization.h\",\n      \"plannedFileSummary\": \"Provides generic serialization and deserialization functions to/from JSON using simdjson and boost::describe.\"\n    },\n    {\n      \"plannedFileDependencies\": [\"json_serialization.h\"],\n      \"plannedFileName\": \"clickhouse_interface.h\",\n      \"plannedFileSummary\": \"Interfaces with ClickHouse, including functions for inserting data, creating tables, and generating query strings.\"\n    },\n    {\n      \"plannedFileDependencies\": [\"json_serialization.h\", \"clickhouse_interface.h\"],\n      \"plannedFileName\": \"market_data.h\",\n      \"plannedFileSummary\": \"Defines data structures for trade streams, aggregate trade streams, book data, and best price data.\"\n    },\n    {\n      \"plannedFileDependencies\": [\"market_data.h\", \"json_serialization.h\"],\n      \"plannedFileName\": \"websocket_client.h\",\n      \"plannedFileSummary\": \"Manages the Binance websocket connection using libwebsockets, handles incoming messages, and dispatches them for processing.\"\n    },\n    {\n      \"plannedFileDependencies\": [\"websocket_client.h\", \"market_data.h\"],\n      \"plannedFileName\": \"snapshot_manager.h\",\n      \"plannedFileSummary\": \"Handles requesting and processing book snapshots via cpp-httplib, ensuring data consistency based on sequence IDs.\"\n    },\n    {\n      \"plannedFileDependencies\": [\"websocket_client.h\", \"snapshot_manager.h\"],\n      \"plannedFileName\": \"logger.h\",\n      \"plannedFileSummary\": \"Configures and manages application logging using spdlog.\"\n    },\n    {\n      \"plannedFileDependencies\": [\"config.h\", \"websocket_client.h\", \"snapshot_manager.h\", \"logger.h\"],\n      \"plannedFileName\": \"main.h\",\n      \"plannedFileSummary\": \"Contains the main application logic, initializes components, and starts the polling-based event loop.\"\n    }\n  ]\n}>]"
