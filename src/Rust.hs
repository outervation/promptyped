{-# LANGUAGE OverloadedStrings #-}

module Rust where

import BuildSystem
import Control.Monad.Except (throwError)
import Core
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import FileSystem (checkBinaryOnPath, gitInit, gitSetupUser, handleExitCode, runAll, runProcessWithTimeout, toFilePath)
import Relude
import System.Directory qualified as Dir
import System.Exit qualified as Exit
import System.FilePath qualified as FP

maxTestFailLinesToShowFullOutput :: Int
maxTestFailLinesToShowFullOutput = 400 -- Consistent with other backends

eitherToMaybe :: Either Text a -> Maybe Text
eitherToMaybe (Left err) = Just err
eitherToMaybe _          = Nothing

-- | Extracts Rust test names from the `cargo test` summary output.
-- | It looks for a block that starts with "failures:" and extracts the indented lines that follow.
-- | Example input:
-- |   ...
-- |   failures:
-- |       tests::my_failing_test
-- |       another_test_in_main
-- |
-- | Example output: ["tests::my_failing_test", "another_test_in_main"]
extractFailedRustTests :: Text -> [Text]
extractFailedRustTests output =
  -- Use a state machine fold to find the "failures:" block and collect subsequent indented lines.
  -- nub to remove duplicates, reverse because foldl' builds the list backwards.
  L.nub . reverse . snd $ foldl' processLine (False, []) (T.lines output)
  where
    -- State: (isInFailuresBlock, accumulatedTests)
    processLine :: (Bool, [Text]) -> Text -> (Bool, [Text])
    processLine (inBlock, acc) line
      | T.strip line == "failures:" = (True, acc) -- Start of the block
      | inBlock =
          if " " `T.isPrefixOf` line || "\t" `T.isPrefixOf` line
            then -- This is an indented line within the block, likely a test name.
              let testName = T.strip line
              in if not (T.null testName)
                   then (True, testName : acc) -- Continue in block, add test
                   else (True, acc)             -- Empty indented line, stay in block
            else (False, acc) -- Unindented line, the block has ended.
      | otherwise = (False, acc) -- Not in the block and this line isn't the start.


isRustFileExtension :: Text -> Bool
isRustFileExtension fileName = ".rs" `T.isSuffixOf` fileName

setupDirectoryRust ::
  Config ->
  ProjectConfig ->
  IO (Maybe Text)
setupDirectoryRust cfg projectCfg = do
  Dir.createDirectoryIfMissing True projectDir
  Dir.withCurrentDirectory projectDir $ do
    let cargoToml = projectDir FP.</> "Cargo.toml"
    cargoTomlExists <- Dir.doesFileExist cargoToml

    let timeout = secondsToNominalDiffTime 60

    eitherToMaybe <$> runAll
      [ initializeIfNeeded cargoTomlExists timeout,
        addDeps timeout,
        addDocs,
        gitInit projectDir,
        gitSetupUser cfg
      ]
  where
    projectDir = configBaseDir cfg

    initializeIfNeeded :: Bool -> NominalDiffTime -> IO (Either Text ())
    initializeIfNeeded True _ = pure (Right ()) -- Cargo.toml already exists
    initializeIfNeeded False time = do
      -- Initialize a library project. It's more generic.
      eRes <- runProcessWithTimeout time "." [] "cargo" ["init"]
      handleExitCode "'cargo init'" eRes

    addDeps :: NominalDiffTime -> IO (Either Text ())
    addDeps time = do
      let depNames = projectDependencyNames projectCfg
      runAll $ flip map depNames $ \depName -> do
        eRes <- runProcessWithTimeout time "." [] "cargo" ["add", T.unpack depName]
        handleExitCode ("'cargo add " <> depName <> "'") eRes

    addDocs :: IO (Either Text ())
    addDocs = do
      forM_ (projectInitialFiles projectCfg) $ uncurry TIO.writeFile
      -- `cargo init` creates src/lib.rs, so we don't need a default main file.
      pure $ Right ()

checkFormatRust ::
  NominalDiffTime ->
  FilePath ->
  IO (Maybe Text)
checkFormatRust timeout dir = Dir.withCurrentDirectory dir $ do
  formatResult <- runProcessWithTimeout timeout "." [] "cargo" ["fmt"]
  res <- handleExitCode "'cargo fmt'" formatResult
  case res of
    Left err -> pure . Just $ "The change you attempted to make would produce invalid syntax. 'cargo fmt' failed with:\n" <> err <> "\nThe change has been rejected. Please ensure your edits result in valid Rust code."
    Right () -> pure Nothing

buildProjectRust ::
  NominalDiffTime ->
  FilePath ->
  [(String, String)] ->
  IO (Maybe Text)
buildProjectRust timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Building Rust project in dir " <> toText dir
  let buildIt = do
        buildResult <- runProcessWithTimeout timeout "." newEnv "cargo" ["build"]
        handleExitCode "'cargo build'" buildResult
  let buildTests = do
        buildResult <- runProcessWithTimeout timeout "." newEnv "cargo" ["nextest", "run", "--no-run"]
        handleExitCode "'cargo nextest run --no-run'" buildResult        
  eitherToMaybe <$> runAll [buildIt, buildTests]

runTestsRust ::
  NominalDiffTime ->
  FilePath ->
  [(String, String)] ->
  IO (Maybe (Text, NumFailedTests))
runTestsRust timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Running Rust tests in dir " <> toText dir
  let initialArgs = ["nextest", "run"]
  initialTestResult <- runProcessWithTimeout timeout "." newEnv "cargo" initialArgs

  case initialTestResult of
    Left procErrText -> do
      let errMsg = "Cargo test execution ('cargo nextest run') failed: " <> procErrText
      putTextLn errMsg
      pure $ Just (errMsg, NumFailedTests (-1))

    Right (initialExitCode, initialStdout, initialStderr) ->
      case initialExitCode of
        Exit.ExitSuccess -> do
          putTextLn "Cargo tests passed."
          pure Nothing

        Exit.ExitFailure _ -> do
          let combinedOutput = initialStdout <> "\n" <> initialStderr
              totalLines = length $ T.lines combinedOutput
              opNameInitial = "'cargo nextest run'"
              failedTests = extractFailedRustTests combinedOutput
              numFailedTests = length failedTests

          if totalLines <= maxTestFailLinesToShowFullOutput || null failedTests then do
            putTextLn "Cargo tests failed. Output is small or no specific tests extracted, reporting directly."
            eitherRes <- handleExitCode opNameInitial (Right (initialExitCode, initialStdout, initialStderr))
            let res = eitherToMaybe eitherRes
            pure $ (\x -> (x, NumFailedTests numFailedTests)) <$> res
          else do
            putTextLn $ "Cargo tests failed. Output has " <> T.pack (show totalLines) <> " lines. Extracted " <> T.pack (show numFailedTests) <> " failed tests. Attempting to re-run them one by one."
            putTextLn $ "Extracted failing tests: " <> T.intercalate ", " failedTests
            res <- tryRerunRustOneByOne timeout newEnv failedTests (initialStdout <> "\n" <> initialStderr) numFailedTests
            pure res

tryRerunRustOneByOne ::
  NominalDiffTime ->
  [(String, String)] ->
  [Text] -> -- List of test names to try
  Text -> -- Original full output, for context if all re-runs pass
  Int -> -- Original number of failed tests
  IO (Maybe (Text, NumFailedTests))
tryRerunRustOneByOne _ _ [] originalOutput numOriginalFailures = do
  let msg = "Initial tests failed with " <> T.pack (show numOriginalFailures) <> " distinct test failures, but all extracted failing tests passed when re-run individually. This suggests a test interaction issue or a flaky test. Original combined output:\n" <> originalOutput
  putTextLn msg
  pure $ Just (msg, NumFailedTests numOriginalFailures)

tryRerunRustOneByOne currentTimeout currentNewEnv (testNameToRun : restTestsToTry) originalOutput numOriginalFailures = do
  -- `cargo test <test_name>` runs only tests containing that name.
  -- Adding `-- --exact` makes it match the full test path, which is what we want.
  let rerunArgs = ["nextest", "run", T.unpack testNameToRun, "--", "--exact"]
  let opNameRerun = "'cargo nextest run" <> testNameToRun <> " -- --exact'"

  putTextLn $ "Attempting to re-run individually: " <> T.unwords (map T.pack rerunArgs)
  rerunResultOrError <- runProcessWithTimeout currentTimeout "." currentNewEnv "cargo" rerunArgs

  case rerunResultOrError of
    Left rerunProcErrText -> do
      let errMsg = "Error (process execution) occurred trying to re-run test " <> testNameToRun <> ": " <> rerunProcErrText
      putTextLn errMsg
      pure $ Just (errMsg, NumFailedTests numOriginalFailures)

    Right (rerunExitCode, rerunStdout, rerunStderr) -> do
      maybeTestFailureErr <- eitherToMaybe <$> handleExitCode opNameRerun (Right (rerunExitCode, rerunStdout, rerunStderr))

      case maybeTestFailureErr of
        Just errMessageFromHandleExitCode -> do
          putTextLn $ "Test " <> testNameToRun <> " failed when re-run individually. Reporting this failure."
          pure $ Just ("Re-running " <> testNameToRun <> " (one of " <> T.pack (show numOriginalFailures) <> " original failures) resulted in:\n" <> errMessageFromHandleExitCode, NumFailedTests numOriginalFailures)
        Nothing -> do
          putTextLn $ "Test " <> testNameToRun <> " passed when re-run individually. Trying next."
          tryRerunRustOneByOne currentTimeout currentNewEnv restTestsToTry originalOutput numOriginalFailures

addRustDependency :: NominalDiffTime -> FilePath -> [(String, String)] -> Text -> IO (Maybe Text)
addRustDependency timeout dir newEnv depName = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Adding Rust dependency " <> depName <> " using cargo."
  result <- runProcessWithTimeout timeout "." newEnv "cargo" ["add", T.unpack depName]
  eitherToMaybe <$> handleExitCode ("'cargo add " <> depName <> "'") result

minimiseRustFileIO :: FilePath -> FilePath -> [(String, String)] -> IO (Either Text Text)
minimiseRustFileIO baseDir path env = Dir.withCurrentDirectory baseDir $ do
  let timeout = 5
  -- Assuming a rustfile_summariser binary exists, following the pattern.
  res <- runProcessWithTimeout timeout "." env "rustfile_summariser" [path]
  case res of
    Left err -> pure . Left $ "Error minimising Rust file " <> T.pack path <> ": " <> err
    Right (exitCode, stdoutRes, stderrRes) ->
      case exitCode of
        Exit.ExitSuccess -> pure $ Right stdoutRes
        Exit.ExitFailure code ->
          pure
            . Left
            $ "Failed to minimise rust file "
            <> T.pack path
            <> " with exit code "
            <> show code
            <> "\nstdout:\n"
            <> truncateText 40 stdoutRes
            <> "\nstderr:\n"
            <> truncateText 40 stderrRes

minimiseRustFile :: Text -> AppM (Either Text Text)
minimiseRustFile pathText = do
  cfg <- ask
  envVars <- getEnvVars
  unless (isRustFileExtension pathText) $
    throwError $ "Error: can only minimise Rust source files (.rs), not " <> pathText
  minimiserExists <- liftIO $ checkBinaryOnPath "rustfile_summariser" envVars
  unless minimiserExists $ throwError "Error: missing rustfile_summariser binary on path."
  let baseDir = configBaseDir cfg
      filePath = toFilePath cfg pathText
  res <- liftIO $ minimiseRustFileIO baseDir filePath envVars
  case res of
    Left err -> pure $ Left $ "Error minimising rust file: " <> err
    Right txt -> pure $ Right txt

isRustTestFile :: Text -> AppM Bool
isRustTestFile fileName = pure $
  if not (isRustFileExtension fileName)
    then False
    else
      -- A common convention is to place integration tests in a `tests` directory.
      let isInTestDir = "tests" `elem` FP.splitDirectories (T.unpack fileName)
      in isInTestDir

data Rust = Rust

instance BuildSystem Rust where
  buildProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ buildProjectRust timeout baseDir envVars

  testProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = 40 -- secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ runTestsRust timeout baseDir (("RUST_BACKTRACE", "1") : envVars)

  setupProject cfg projectCfg = liftIO $ setupDirectoryRust cfg projectCfg

  isBuildableFile fileName = pure $ isRustFileExtension fileName

  isTestFile = isRustTestFile

  getIgnoredDirs = pure ["target", ".git", "assets"]

  getFormatChecker cfg = do
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    pure $ checkFormatRust timeout baseDir

  minimiseFile = minimiseRustFile

  addDependency name = do
    cfg <- ask
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ addRustDependency timeout baseDir envVars name

  -- Rust uses C-style comments, so we can reuse the functions from Core.
  addLineNumberComment num txt = addCppStyleLineNumberComment num txt
  removeLineNumberCommentIfPresent txt = removeCppStyleLineNumberComment txt
  fileExtension = ".rs"

