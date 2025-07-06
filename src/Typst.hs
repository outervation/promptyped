
{-# LANGUAGE OverloadedStrings #-}

module Typst where

import BuildSystem
import Control.Monad.Except (throwError)
import Core
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import FileSystem (checkBinaryOnPath, getFileNamesRecursive, gitInit, gitSetupUser, handleExitCode, runAll, runProcessWithTimeout, toFilePath, readFileToText)
import Relude
import System.Directory qualified as Dir
import System.FilePath qualified as FP

isTypstFileExtension :: Text -> Bool
isTypstFileExtension fileName = ".typ" `T.isSuffixOf` fileName

-- | Sets up a new Typst project directory.
-- Checks for the 'typst' binary and initializes a git repository.
setupDirectoryTypst :: Config -> ProjectConfig -> IO (Maybe Text)
setupDirectoryTypst cfg _projectCfg = do
  let baseDir = configBaseDir cfg
  typstExists <- checkBinaryOnPath "typst" []
  case typstExists of
    False -> return $ Just "The 'typst' binary was not found on your PATH. Please install it to continue."
    True -> do
      gitInitRes <- gitInit baseDir
      case gitInitRes of
        Left err -> pure $ Just err
        Right () -> do
          gitUserRes <- gitSetupUser cfg
          case gitUserRes of
            Left err -> pure $ Just err
            Right () -> pure Nothing

-- | Compiles all .typ files in the project directory.
buildProjectTypst :: NominalDiffTime -> FilePath -> [(String, String)] -> IO (Maybe Text)
buildProjectTypst timeout dir newEnv = do
  allFiles <- getFileNamesRecursive [".git"] dir
  let typFiles = filter isTypstFileExtension allFiles
  case typFiles of
    [] -> pure Nothing -- No files to compile is not an error.
    _ -> do
      let compileActions = map (compileFile timeout dir newEnv . T.unpack) typFiles
      result <- runAll compileActions
      pure $ either Just (const Nothing) result

-- | Helper to compile a single Typst file.
compileFile :: NominalDiffTime -> FilePath -> [(String, String)] -> FilePath -> IO (Either Text ())
compileFile timeout dir newEnv entryPoint = do
    -- The entryPoint is relative to `dir`, which is our working directory for the process.
    res <- runProcessWithTimeout timeout dir newEnv "typst" ["compile", entryPoint]
    handleExitCode ("typst compile " <> toText entryPoint) res

-- | Formats all Typst files in the directory recursively.
checkFormatTypst :: NominalDiffTime -> FilePath -> IO (Maybe Text)
checkFormatTypst timeout dir = do
  res <- runProcessWithTimeout timeout dir [] "typst" ["fmt", "."]
  fmap (either Just (const Nothing)) (handleExitCode "typst fmt" res)

-- | A no-op minimizer that returns the file content unchanged.
minimiseTypstFile :: Text -> AppM (Either Text Text)
minimiseTypstFile pathText = do
    cfg <- ask
    let fp = toFilePath cfg pathText
    content <- liftIO $ readFileToText fp
    pure $ Right content

data Typst = Typst

instance BuildSystem Typst where
  buildProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ buildProjectTypst timeout baseDir envVars

  testProject _ = pure Nothing -- Unit tests don't make sense for Typst, so always succeed.

  setupProject cfg projectCfg = liftIO $ setupDirectoryTypst cfg projectCfg

  isBuildableFile = pure . isTypstFileExtension

  isTestFile _ = pure False -- No concept of test files for Typst in this context.

  getIgnoredDirs = pure [".git"]

  getFormatChecker cfg = do
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    pure $ checkFormatTypst timeout baseDir

  minimiseFile = minimiseTypstFile

  addDependency _ = throwError "Dependency management not supported for Typst in this context."

  -- Typst uses C-style comments for line/block comments.
  addLineNumberComment = addCppStyleLineNumberComment
  removeLineNumberCommentIfPresent = removeCppStyleLineNumberComment

  fileExtension = ".typ"
