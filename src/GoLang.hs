{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GoLang where

import BuildSystem
import Control.Monad.Except
import Core
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import FileSystem (checkBinaryOnPath, gitInit, gitSetupUser, handleExitCode, runAll, runProcessWithTimeout, toFilePath, readFileToText)
import Relude
import System.Directory qualified as Dir
import System.Exit qualified as Exit
import System.FilePath qualified as FP
import Text.RawString.QQ (r)

maxTestFailLinesToShowFullOutput :: Int
maxTestFailLinesToShowFullOutput = 600

eitherToMaybe :: Either Text () -> Maybe Text
eitherToMaybe (Left err) = Just err
eitherToMaybe _ = Nothing

-- | Extracts Go test names from lines like "--- FAIL: TestName (duration)"
-- Example input line: "--- FAIL: TestFeatureCursor_T1_Calculation (0.01s)"
-- Example output: "TestFeatureCursor_T1_Calculation"
extractFailedGoTests :: Text -> [Text]
extractFailedGoTests output =
  mapMaybe parseFailLine (T.lines output)
  where
    parseFailLine :: Text -> Maybe Text
    parseFailLine line =
      let strippedLine = T.strip line -- Strip leading/trailing whitespace from the whole line
      in case T.stripPrefix "--- FAIL:" strippedLine of
        Just restAfterFailMarker ->
          -- The test name is the first "word" after "--- FAIL: "
          -- T.strip restAfterFailMarker will remove leading spaces if any between ":" and test name
          case T.words (T.strip restAfterFailMarker) of
            (testName : _) -> Just testName
            _              -> Nothing -- Unexpected format after "--- FAIL:"
        Nothing -> Nothing -- Line doesn't start with "--- FAIL:" (after initial stripping)

tmpp :: IO [Text]
tmpp = do
  contents <- readFileToText "/home/dfxs/tmpout.txt"
  --putTextLn contents
  return $ extractFailedGoTests contents

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
  fmtResult <- runProcessWithTimeout timeout "." [] "go" ["fmt", "./..."]
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
        fmtResult <- runProcessWithTimeout timeout "." newEnv "go" ["fmt", "./..."]
        handleExitCode "'go fmt'" fmtResult
      generateIt = do
        generateResult <- runProcessWithTimeout timeout "." newEnv "go" ["generate", "./..."]
        handleExitCode "'go generate'" generateResult
  eitherToMaybe <$> runAll [generateIt, buildIt, fmtIt]

runTestsGoOld ::
  -- | Timeout in seconds
  NominalDiffTime ->
  -- | Project directory
  FilePath ->
  -- | Environment variables
  [(String, String)] ->
  IO (Maybe Text)
runTestsGoOld timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Testing Go project in dir " <> toText dir
  testResult <- runProcessWithTimeout timeout "." newEnv "go" ["test", "-timeout", "30s", "./..."]
  eitherToMaybe <$> handleExitCode "'go test'" testResult

runTestsGo ::
  NominalDiffTime ->
  FilePath ->
  [(String, String)] ->
  IO (Maybe Text)
runTestsGo timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Running initial Go tests in dir " <> toText dir
  let initialArgs = ["test", "-timeout", "30s", "./..."] 
  initialTestResult <- runProcessWithTimeout timeout "." newEnv "go" initialArgs

  case initialTestResult of
    Left procErr -> pure $ Just procErr

    Right (initialExitCode, initialStdout, initialStderr) ->
      case initialExitCode of
        Exit.ExitSuccess -> do
          putTextLn "Initial Go tests passed."
          pure Nothing

        Exit.ExitFailure _ -> do
          let combinedOutput = initialStdout <> "\n" <> initialStderr
          let totalLines = length $ T.lines combinedOutput
          let opNameInitial = "'go test ./...'" 

          if totalLines <= maxTestFailLinesToShowFullOutput then do
            putTextLn $ "Initial Go tests failed. Output is small, reporting directly."
            eitherToMaybe <$> handleExitCode opNameInitial (Right (initialExitCode, initialStdout, initialStderr))
          else do
            putTextLn $ "Initial Go tests failed. Output has " <> T.pack (show totalLines) <> " lines. Attempting to extract and re-run just one test."
            --let failedTests = nub $ extractFailedGoTests combinedOutput
            let failedTests = take 1 $ extractFailedGoTests combinedOutput

            case failedTests of
              [] -> do
                let err = "Could not extract specific failed test names (might be a compile error or different output format). Please panic!"
                pure $ Just err
                --pure $ eitherToMaybe $ handleExitCode opNameInitial (initialExitCode, initialStdout, initialStderr)

              tests -> do
                -- Go's -run flag takes a regex. Join test names with '|' for an OR match.
                -- Wrap with ^ and $ for exact matches of the test names.
                -- We need to escape characters that are special in regex if they appear in test names,
                -- but typically Go test names are alphanumeric + underscores.
                -- For simplicity, we assume test names don't need further regex escaping here.
                let runRegex = "^(" <> T.intercalate "|" tests <> ")$"
                let rerunArgs = ["test", "-run", T.unpack runRegex, "-timeout", "30s", "./..."] 
                let opNameRerun = "'go test -run=" <> runRegex <> "'"

                putTextLn $ "Re-running failed tests with: go " <> unwords (map T.pack rerunArgs)
                rerunResult <- runProcessWithTimeout timeout "." newEnv "go" rerunArgs

                case rerunResult of
                  Left rerunProcErr -> do
                     putTextLn $ "Error occurred trying to re-run failed tests: " <> rerunProcErr
                     pure $ Just rerunProcErr
                  Right (rerunExitCode, rerunStdout, rerunStderr) -> do
                     putTextLn "Reporting result from the re-run of failed tests."
                     eitherToMaybe <$> handleExitCode opNameRerun (Right (rerunExitCode, rerunStdout, rerunStderr))

addGoDependency :: NominalDiffTime -> FilePath ->  [(String, String)]  -> Text -> IO (Maybe Text)
addGoDependency timeout dir newEnv name = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Adding go dependency " <> name
  result <- runProcessWithTimeout timeout "." newEnv "go" ["get", T.unpack name]
  eitherToMaybe <$> handleExitCode "'go get'" result

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

minimiseGoFile :: Text -> AppM (Either Text Text)
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
    Left err -> pure $ Left $ "Error minimising go file: " <> err
    Right txt -> pure $ Right txt

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

  addDependency name = do
    cfg <- ask
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    envVars <- getEnvVars
    liftIO $ addGoDependency timeout baseDir envVars name
