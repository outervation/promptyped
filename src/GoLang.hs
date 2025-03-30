{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GoLang where

import BuildSystem
import Control.Monad.Except
import Core
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import FileSystem (checkBinaryOnPath, gitInit, gitSetupUser, handleExitCode, runAll, runProcessWithTimeout, toFilePath)
import Relude
import System.Directory qualified as Dir
import System.Exit qualified as Exit
import System.FilePath qualified as FP

eitherToMaybe :: Either Text () -> Maybe Text
eitherToMaybe (Left err) = Just err
eitherToMaybe _ = Nothing

setupDirectoryGo ::
  -- | Path to the project directory
  Config ->
  ProjectConfig ->
  IO (Maybe Text)
setupDirectoryGo cfg projectCfg = do
  Dir.createDirectoryIfMissing True path
  Dir.withCurrentDirectory path $ do
    let goModFile = path FP.</> "go.mod"
    alreadyExists <- Dir.doesFileExist goModFile
    eitherToMaybe
      <$> runAll
        [ makeIfNotAlreadyExists alreadyExists,
          addDeps,
          addDocs,
          gitInit path,
          gitSetupUser cfg
        ]
  where
    path = configBaseDir cfg
    timeout = 60

    tidyIt = do
      eTidy <- runProcessWithTimeout timeout "." [] "go" ["mod", "tidy"]
      handleExitCode "'go mod tidy'" eTidy

    makeIfNotAlreadyExists :: Bool -> IO (Either Text ())
    makeIfNotAlreadyExists alreadyExists =
      if alreadyExists
        then pure (Right ())
        else do
          let modName = FP.takeFileName (FP.dropTrailingPathSeparator path)
          eRes <- runProcessWithTimeout timeout "." [] "go" ["mod", "init", modName]
          handleExitCode ("'go mod init' " <> toText modName) eRes

    addDeps :: IO (Either Text ())
    addDeps = do
      let depNames = projectDependencyNames projectCfg
          results = flip map depNames $ \x -> do
            eRes <- runProcessWithTimeout timeout "." [] "go" ["get", T.unpack x]
            handleExitCode ("'go get' " <> x) eRes
      runAll results

    addDocs :: IO (Either Text ())
    addDocs = do
      forM_ (projectInitialFiles projectCfg) $ \(name, txt) -> TIO.writeFile name txt
      TIO.writeFile "main.go" "package main \n \n func main() { }"
      return $ Right ()

checkFormatGo ::
  -- | Timeout in seconds
  NominalDiffTime ->
  -- | Project directory
  FilePath ->
  IO (Maybe Text)
checkFormatGo timeout dir = Dir.withCurrentDirectory dir $ do
  fmtResult <- runProcessWithTimeout timeout "." [] "go" ["fmt"]
  res <- handleExitCode "'go fmt'" fmtResult
  case res of
    Left err -> pure . Just $ "The change you attempted to make would produce invalid syntax, with go fmt failing with:\n" <> err <> "\nThe change has been rejected so please try again, being careful with line numbers (remembering they're inclusive; [startLine, endLine]). An off-by-one error for instance might accidentally delete the closing bracket of a previous function, or fail to replace the final bracket of a function being replaced (leading to duplicate closing brackets)."
    Right () -> pure Nothing

buildProjectGo ::
  -- | Timeout in seconds
  NominalDiffTime ->
  -- | Project directory
  FilePath ->
  -- | Environment variables
  [(String, String)] ->
  IO (Maybe Text)
buildProjectGo timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Building Go project in dir " <> toText dir
  let buildIt = do
        buildResult <- runProcessWithTimeout timeout "." newEnv "go" ["build", "./..."]
        handleExitCode "'go build'" buildResult
      fmtIt = do
        fmtResult <- runProcessWithTimeout timeout "." newEnv "go" ["fmt"]
        handleExitCode "'go fmt'" fmtResult
  eitherToMaybe <$> runAll [buildIt, fmtIt]

runTestsGo ::
  -- | Timeout in seconds
  NominalDiffTime ->
  -- | Project directory
  FilePath ->
  -- | Environment variables
  [(String, String)] ->
  IO (Maybe Text)
runTestsGo timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Testing Go project in dir " <> toText dir
  testResult <- runProcessWithTimeout timeout "." newEnv "go" ["test", "-timeout", "40s", "./..."]
  eitherToMaybe <$> handleExitCode "'go test'" testResult

isGoFileExtension :: Text -> Bool
isGoFileExtension fileName = ".go" `T.isSuffixOf` fileName

minimiseGoFileIO :: FilePath -> FilePath -> [(String, String)] -> IO (Either Text Text)
minimiseGoFileIO baseDir path env = Dir.withCurrentDirectory baseDir $ do
  let timeout = 5
  res <- runProcessWithTimeout timeout "." env "gofile_summariser" [path]
  case res of
    Left err -> pure . Left $ "Error minimising Go file " <> (T.pack path) <> ": " <> err
    Right (exitCode, stdoutRes, stderrRes) ->
      case exitCode of
        Exit.ExitSuccess -> pure $ Right stdoutRes
        Exit.ExitFailure code ->
          pure
            . Left
            $ "Failed to minimise go file "
            <> (T.pack path)
            <> " with exit code "
            <> show code
            <> "\nstdout:\n"
            <> truncateText 40 stdoutRes
            <> "\nstderr:\n"
            <> truncateText 40 stderrRes

minimiseGoFile :: Text -> AppM Text
minimiseGoFile path = do
  cfg <- ask
  envVars <- getEnvVars
  unless (isGoFileExtension path) $ throwError $ "Error: can only minimise Go source files, not " <> path
  minimiserExists <- liftIO $ checkBinaryOnPath "gofile_summariser" envVars
  unless minimiserExists $ throwError $ "Error: missing gofile_summariser binary on path; need to go install https://github.com/outervation/gofile_summariser"
  let baseDir = configBaseDir cfg
      filePath = toFilePath cfg path
  res <- liftIO $ minimiseGoFileIO baseDir filePath envVars
  case res of
    Left err -> throwError $ "Error minimising go file: " <> err
    Right txt -> pure txt

data GoLang = GoLang

instance BuildSystem GoLang where
  buildProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ buildProjectGo timeout baseDir envVars

  testProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ runTestsGo timeout baseDir envVars

  setupProject cfg projectCfg = liftIO $ setupDirectoryGo cfg projectCfg

  isBuildableFile fileName = pure $ isGoFileExtension fileName

  getIgnoredDirs = pure ["build", ".git", "contrib"]

  getFormatChecker cfg = do
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    return $ checkFormatGo timeout baseDir

  minimiseFile = minimiseGoFile
