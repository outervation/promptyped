{-# LANGUAGE OverloadedStrings #-}

module Memoise where

import Control.Monad.Except
import Core
import Data.Aeson
import Data.Group ((~~))
import Data.List ((\\))
import Data.Text qualified as T
import Relude
import System.Directory qualified as DIR
import System.FilePath qualified as FP

data CacheData b = CacheData
  { cachedResult :: b,
    cachedMetricsDelta :: Metrics,
    cachedOpenFiles :: [OpenFile],
    cachedExistingFiles :: [ExistingFile],
    cachedCompileTestRes :: CompileTestState
  }
  deriving (Generic, Show)

instance (ToJSON b) => ToJSON (CacheData b)

instance (FromJSON b) => FromJSON (CacheData b)

loadFromCache :: (FromJSON b) => FilePath -> AppM b
loadFromCache fileName = do
  eitherCache <- liftIO $ eitherDecodeFileStrict' fileName
  case eitherCache of
    Left err -> throwError $ "Failed to parse cache at " <> show fileName <> ": " <> show err
    Right (CacheData b metricsDelta openFiles files compileTestRes) -> do
      -- Update metrics with delta
      modify' $ \s ->
        s
          { stateMetrics = stateMetrics s <> metricsDelta,
--            stateOpenFiles = mergeOpenFiles (stateOpenFiles s) openFiles,
--            stateFiles = mergeExistingFiles (stateFiles s) files,
            stateCompileTestRes = compileTestRes
          }
      return b

mergeOpenFiles :: [OpenFile] -> [OpenFile] -> [OpenFile]
mergeOpenFiles existing new =
  -- Remove existing files that have names matching any new files
  let existingFiltered = filter (\e -> openFileName e `notElem` map openFileName new) existing
   in -- Then append all new files
      existingFiltered ++ new

mergeExistingFiles :: [ExistingFile] -> [ExistingFile] -> [ExistingFile]
mergeExistingFiles existing new =
  let existingFiltered = filter (\e -> existingFileName e `notElem` map existingFileName new) existing
   in existingFiltered ++ new

computeAndCache :: (ToJSON b) => FilePath -> a -> (a -> AppM b) -> AppM b
computeAndCache fileName input fn = do
  -- Capture initial state
  initialMetrics <- stateMetrics <$> get
  initialOpenFiles <- stateOpenFiles <$> get
  initialFiles <- stateFiles <$> get

  -- Run the computation
  result <- fn input

  -- Capture final state
  finalState <- get
  let finalMetrics = stateMetrics finalState
      finalOpenFiles = stateOpenFiles finalState
      finalFiles = stateFiles finalState

  -- Calculate deltas
  let metricsDelta = finalMetrics ~~ initialMetrics
      openFilesDelta = finalOpenFiles \\ initialOpenFiles
      filesDelta = finalFiles \\ initialFiles

  -- Write cache
  liftIO $ do
    DIR.createDirectoryIfMissing True (FP.takeDirectory fileName)
    encodeFile fileName
      $ CacheData
        { cachedResult = result,
          cachedMetricsDelta = metricsDelta,
          cachedOpenFiles = openFilesDelta,
          cachedExistingFiles = filesDelta,
          cachedCompileTestRes = stateCompileTestRes finalState
        }

  return result

memoise :: (FromJSON b, ToJSON b) => FilePath -> Text -> a -> (a -> Text) -> (a -> AppM b) -> AppM b
memoise storageDir kind input format fn = do
  let sanitizedKind = T.unpack kind
      fileName = storageDir FP.</> sanitizedKind ++ "_" ++ T.unpack (format input) ++ ".json"

  -- Check if cache exists
  cacheExists <- liftIO $ DIR.doesFileExist fileName
  if cacheExists
    then loadFromCache fileName
    else computeAndCache fileName input fn
