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
          { metricsTokensIn = 5,
            metricsTokensOut = 3,
            metricsCost = 7,
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
            configModel = "chatgpt3.5",
            configBaseDir = "./",
            configCacheDir = "./",
            configBuildTimeoutSeconds = 60,
            configBuildNumJobs = 1,
            configGitUserName = "",
            configGitUserEmail = "",
            configTaskMaxFailures = 1,
            configForbiddenFiles = []
          }
      initialState =
        AppState
          { stateMetrics = mempty,
            stateOpenFiles = [],
            stateFiles = [],
            stateCompileTestRes = CompileTestState Nothing Nothing
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
  metricsTokensIn (stateMetrics finalState2) `shouldBe` metricsTokensIn expectedMetrics
  metricsTokensOut (stateMetrics finalState2) `shouldBe` metricsTokensOut expectedMetrics
  metricsCost (stateMetrics finalState2) `shouldBe` metricsCost expectedMetrics
  metricsApiTime (stateMetrics finalState2) `shouldBe` metricsApiTime expectedMetrics

  -- Verify file lists
  let expectedFile = ExistingFile (T.pack testFileName) (T.pack testDesc)
      expectedFiles
        | useExistingCache = [expectedFile]
        | otherwise = [expectedFile, expectedFile]

  stateFiles finalState2 `shouldBe` nub expectedFiles

  -- Verify open files
  let expectedOpenFile = OpenFile (T.pack testFileName) (T.pack testContent)
      expectedOpenFiles
        | useExistingCache = [expectedOpenFile]
        | otherwise = [expectedOpenFile, expectedOpenFile]

  stateOpenFiles finalState2 `shouldBe` nub expectedOpenFiles

-- Helper to run AppM computations (same as previous)
runAppMWith :: Config -> AppState -> AppM a -> IO (a, AppState)
runAppMWith config theState app = do
  result <- runExceptT $ runStateT (runReaderT (runAppM app) config) theState
  either (error . show) return result
