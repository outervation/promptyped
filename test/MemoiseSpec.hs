{-# LANGUAGE OverloadedStrings #-}

module MemoiseSpec where

import Control.Exception (catch)
import Core
import Data.List (nub)
import Data.Text qualified as T
import Memoise
import Relude
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.IO.Error (IOError)
import Test.Hspec

main :: IO ()
main = hspec spec

cacheName :: FilePath
cacheName = "memoise_test_cache"

spec :: Spec
spec = beforeAll cleanup $ afterAll (const cleanup) $ do
  describe "memoise" $ do
    it "computes and caches results with state deltas" $ do
      testWithCache False
    it "uses cached results and applies deltas" $ do
      testWithCache True

cleanup :: IO ()
cleanup = removeDirectoryRecursive cacheName `catch` \(_ :: IOError) -> pure ()

testWithCache :: Bool -> IO ()
testWithCache useExistingCache = do
  createDirectoryIfMissing True cacheName
  callRef <- newIORef (0 :: Int)

  let addedMetrics =
        Metrics
          { metricsLowInt = mempty,
            metricsMediumInt = mempty,
            metricsHighInt = mempty,
            metricsApiTime = 9,
            metricsCompileTime = 0,
            metricsTestTime = 0,
            metricsNumSyntaxErrors = 0,
            metricsNumCompileFails = 0,
            metricsNumTestFails = 0
          }

  let testFileName = "test_file"
      testContent = "test_content"
      testDesc = "test_description"

      fn :: String -> AppM String
      fn _ = do
        liftIO $ modifyIORef callRef (+ 1)
        -- Modify state with proper file instances
        modify $ \s ->
          s
            { stateMetrics =
                stateMetrics s <> addedMetrics,
              stateOpenFiles =
                OpenFile
                  { openFileName = T.pack testFileName,
                    openFileContents = T.pack testContent
                  }
                  : stateOpenFiles s,
              stateFiles =
                ExistingFile
                  { existingFileName = T.pack testFileName,
                    existingFileDesc = T.pack testDesc
                  }
                  : stateFiles s
            }
        pure "result"

      config =
        Config
          { configApiKey = "",
            configApiSite = "openrouter.ai",
            configLowIntModel = "chatgpt3.5",
            configMediumIntModel = "chatgpt3.5",
            configHighIntModel = "chatgpt3.5",
            configBaseDir = "./",
            configCacheDir = "./",
            configBuildTimeoutSeconds = 60,
            configBuildNumJobs = 1,
            configEnvVars = [],
            configGitUserName = "",
            configGitUserEmail = "",
            configTaskMaxFailures = RemainingFailureTolerance 1 1,
            configForbiddenFiles = [],
            configModelTemperature = Nothing,
            configModelMaxInputTokens = 10000
          }
      initialState =
        AppState
          { stateMetrics = mempty,
            stateOpenFiles = [],
            stateFiles = [],
            stateCompileTestRes = CompileTestState Nothing Nothing 0 0
          }

  -- First run (always compute)
  (result1, finalState1) <-
    runAppMWith config initialState
      $ memoise cacheName "test" "input" show fn

  -- Second run (uses cache if useExistingCache)
  let initState2 = if useExistingCache then initialState else finalState1
  (result2, finalState2) <-
    runAppMWith config initState2
      $ memoise cacheName "test" "input" show fn

  -- Verify call count
  callCount <- readIORef callRef
  if useExistingCache
    then callCount `shouldBe` 0 -- Function should not be called again
    else callCount `shouldBe` 1 -- Function should only be called once

  -- Verify results
  result1 `shouldBe` "result"
  result2 `shouldBe` "result"

  -- Verify metrics changes
  let expectedMetrics
        | useExistingCache = addedMetrics -- Only first computation
        | otherwise = addedMetrics <> addedMetrics -- Both computations
  metricsLowInt (stateMetrics finalState2) `shouldBe` metricsLowInt expectedMetrics
  metricsMediumInt (stateMetrics finalState2) `shouldBe` metricsLowInt expectedMetrics
  metricsHighInt (stateMetrics finalState2) `shouldBe` metricsLowInt expectedMetrics  
  metricsApiTime (stateMetrics finalState2) `shouldBe` metricsApiTime expectedMetrics

  -- Verify file lists
  let expectedFile = ExistingFile (T.pack testFileName) (T.pack testDesc)
      expectedFiles
        | useExistingCache = []
        -- \| useExistingCache = [expectedFile] Now we don't load files from cache
        | otherwise = [expectedFile, expectedFile]

  stateFiles finalState2 `shouldBe` nub expectedFiles

  -- Verify open files
  let expectedOpenFile = OpenFile (T.pack testFileName) (T.pack testContent) (T.pack testContent) True 0
      expectedOpenFiles
        | useExistingCache = []
        -- \| useExistingCache = [expectedOpenFile] Now we don't load files from cache
        | otherwise = [expectedOpenFile, expectedOpenFile]

  stateOpenFiles finalState2 `shouldBe` nub expectedOpenFiles

-- Helper to run AppM computations (same as previous)
runAppMWith :: Config -> AppState -> AppM a -> IO (a, AppState)
runAppMWith config theState app = do
  result <- runExceptT $ runStateT (runReaderT (runAppM app) config) theState
  either (error . show) return result
