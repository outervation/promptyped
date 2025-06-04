-- PythonLang.hs
{-# LANGUAGE OverloadedStrings #-}

module Python where

import BuildSystem
import Control.Monad.Except (throwError) -- For AppM
import Core
import Data.Char (isDigit)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import FileSystem (gitInit, gitSetupUser, handleExitCode, runAll, runProcessWithTimeout, toFilePath, checkBinaryOnPath)
import Relude 
import System.Directory qualified as Dir
import System.Exit qualified as Exit
import System.FilePath qualified as FP

maxTestFailLinesToShowFullOutput :: Int
maxTestFailLinesToShowFullOutput = 400 -- Same as GoLang

eitherToMaybe :: Either Text a -> Maybe Text
eitherToMaybe (Left err) = Just err
eitherToMaybe _          = Nothing

-- | Extracts Pytest test names from lines like "FAILED tests/sample_test.py::test_pytest_is_working - ..."
extractFailedPytestTests :: Text -> [Text]
extractFailedPytestTests output =
  L.nub $ mapMaybe parseFailLine (T.lines output)
  where
    parseFailLine :: Text -> Maybe Text
    parseFailLine line =
      let strippedLine = T.stripStart line -- Don't strip trailing spaces yet
      in case T.stripPrefix "FAILED " strippedLine of
        Just rest ->
          -- The test name is between "FAILED " and " - "
          case T.splitOn " - " rest of
            (testName : _) -> Just $ T.strip testName
            _              -> Nothing -- Line started with "FAILED " but no " - " found
        Nothing -> Nothing -- Line doesn't start with "FAILED "

isPythonFileExtension :: Text -> Bool
isPythonFileExtension fileName = ".py" `T.isSuffixOf` fileName || ".pyi" `T.isSuffixOf` fileName

setupDirectoryPython ::
  Config ->
  ProjectConfig ->
  IO (Maybe Text)
setupDirectoryPython cfg projectCfg = do
  Dir.createDirectoryIfMissing True projectDir
  Dir.withCurrentDirectory projectDir $ do
    let pyProjectToml = projectDir FP.</> "pyproject.toml"
    pyProjectTomlExists <- Dir.doesFileExist pyProjectToml

    let timeout = secondsToNominalDiffTime 60 -- Standard timeout for setup operations

    eitherToMaybe <$> runAll
      [ initializeIfNeeded pyProjectTomlExists timeout,
        addDeps timeout,
        addDocs,
        gitInit projectDir,
        gitSetupUser cfg
      ]
  where
    projectDir = configBaseDir cfg

    initializeIfNeeded :: Bool -> NominalDiffTime -> IO (Either Text ())
    initializeIfNeeded True _ = pure (Right ()) -- pyproject.toml already exists
    initializeIfNeeded False time = do
      let projectName = T.pack $ FP.takeFileName (FP.dropTrailingPathSeparator projectDir)
      -- `uv init` can be interactive. We might need non-interactive mode if available,
      -- or ensure defaults are suitable. For now, assume `uv init` works non-interactively.
      -- `uv init --name <projectName>` might be more direct if `uv init` doesn't pick up dir name.
      -- As of uv 0.1.17, `uv init` is non-interactive.
      eRes <- runProcessWithTimeout time "." [] "uv" ["init"]
      handleExitCode ("'uv init " <> projectName <> "'") eRes

    addDeps :: NominalDiffTime -> IO (Either Text ())
    addDeps time = do
      let depNames = projectDependencyNames projectCfg
      runAll $ flip map depNames $ \depName -> do
        eRes <- runProcessWithTimeout time "." [] "uv" ["add", T.unpack depName]
        handleExitCode ("'uv add " <> depName <> "'") eRes

    addDocs :: IO (Either Text ())
    addDocs = do
      let initialFiles = projectInitialFiles projectCfg
      if null initialFiles
        then -- Create a default main.py if no initial files are specified
          TIO.writeFile "main.py" "def main():\n    print(\"Hello from Python!\")\n\nif __name__ == \"__main__\":\n    main()\n"
        else forM_ initialFiles $ \(fp, content) -> do
              let fullPath = fp
              Dir.createDirectoryIfMissing True (FP.takeDirectory fullPath)
              TIO.writeFile fullPath content
      pure $ Right ()

checkFormatPython ::
  NominalDiffTime ->
  FilePath ->
  IO (Maybe Text)
checkFormatPython timeout dir = Dir.withCurrentDirectory dir $ do
  -- `uvx ruff check .` will check all files in the current directory and subdirectories
  -- respecting .ruff.toml or pyproject.toml [tool.ruff] settings.
  formatResult <- runProcessWithTimeout timeout "." [] "uvx" ["ruff", "check", "."]
  res <- handleExitCode "'uvx ruff check .'" formatResult
  case res of
    Left err -> pure . Just $ "The change you attempted to make would produce files that fail linting/formatting. 'uvx ruff check .' failed with:\n" <> err <> "\nThe change has been rejected. Please try again, ensuring your code adheres to the project's linting standards."
    Right () -> pure Nothing

buildProjectPython ::
  NominalDiffTime ->
  FilePath ->
  [(String, String)] ->
  IO (Maybe Text)
buildProjectPython timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Type checking Python project in dir " <> toText dir
  -- `uvx ty check` (assuming `ty` is an alias for mypy or similar, configured in pyproject.toml scripts)
  -- If `ty` isn't a script, this might be `uvx mypy .` if mypy is the intended type checker.
  -- We'll stick to `uvx ty check` as per the prompt.
  buildResult <- runProcessWithTimeout timeout "." newEnv "uvx" ["ty", "check", "."]
  eitherToMaybe <$> handleExitCode "'uvx ty check .'" buildResult

runTestsPython ::
  NominalDiffTime ->
  FilePath ->
  [(String, String)] ->
  IO (Maybe (Text, NumFailedTests))
runTestsPython timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Running Pytest tests in dir " <> toText dir
  let initialArgs = ["pytest"] -- Pytest typically discovers tests automatically
  initialTestResult <- runProcessWithTimeout timeout "." newEnv "uvx" initialArgs

  case initialTestResult of
    Left procErrText -> do
      let errMsg = "Pytest execution ('uvx pytest') failed: " <> procErrText
      putTextLn errMsg
      pure $ Just (errMsg, NumFailedTests (-1))

    Right (initialExitCode, initialStdout, initialStderr) ->
      case initialExitCode of
        Exit.ExitSuccess -> do
          putTextLn "Pytest tests passed."
          pure Nothing

        Exit.ExitFailure _ -> do
          let combinedOutput = initialStdout <> "\n" <> initialStderr
              totalLines = length $ T.lines combinedOutput
              opNameInitial = "'uvx pytest'"
              failedTests = extractFailedPytestTests combinedOutput
              numFailedTests = length failedTests

          if totalLines <= maxTestFailLinesToShowFullOutput || null failedTests then do
            putTextLn "Pytest tests failed. Output is small or no specific tests extracted, reporting directly."
            eitherRes <- handleExitCode opNameInitial (Right (initialExitCode, initialStdout, initialStderr))
            let res = eitherToMaybe eitherRes
            pure $ (\x -> (x, NumFailedTests numFailedTests)) <$> res
          else do
            putTextLn $ "Pytest tests failed. Output has " <> T.pack (show totalLines) <> " lines. Extracted " <> T.pack (show numFailedTests) <> " failed tests. Attempting to re-run them one by one."
            putTextLn $ "Extracted failing tests: " <> T.intercalate ", " failedTests
            -- Rerun the first failed test to provide a focused error message.
            -- Or, adapt tryRerunPytestOneByOne to collect all individual failures if needed.
            -- For now, report the first one that fails individually.
            res <- tryRerunPytestOneByOne timeout newEnv failedTests (initialStdout <> "\n" <> initialStderr) numFailedTests
            pure res


tryRerunPytestOneByOne ::
  NominalDiffTime ->
  [(String, String)] ->
  [Text] -> -- List of test names to try
  Text -> -- Original full output, for context if all re-runs pass
  Int -> -- Original number of failed tests
  IO (Maybe (Text, NumFailedTests)) -- Just (errorText, numFailed) if a test fails or process error
tryRerunPytestOneByOne _ _ [] originalOutput numOriginalFailures = do
  let msg = "Initial tests failed with " <> T.pack (show numOriginalFailures) <> " distinct test failures, but all extracted failing tests passed when re-run individually. This suggests a test interaction issue, a flaky test, or that the extracted test names were not the sole culprits of the original large failure. Original combined output:\n" <> originalOutput
  putTextLn msg
  pure $ Just (msg, NumFailedTests numOriginalFailures) -- Report the original number of failures

tryRerunPytestOneByOne currentTimeout currentNewEnv (testNameToRun : restTestsToTry) _originalOutput numOriginalFailures = do
  -- Pytest can run tests by their node ID, which is what we extract.
  let rerunArgs = ["pytest", T.unpack testNameToRun]
  let opNameRerun = "'uvx pytest " <> testNameToRun <> "'"

  putTextLn $ "Attempting to re-run individually: uvx " <> T.unwords (map T.pack rerunArgs)
  rerunResultOrError <- runProcessWithTimeout currentTimeout "." currentNewEnv "uvx" rerunArgs

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
          putTextLn $ "Test " <> testNameToRun <> " passed when re-run individually. Trying next (if any)."
          tryRerunPytestOneByOne currentTimeout currentNewEnv restTestsToTry _originalOutput numOriginalFailures


addPythonDependency :: NominalDiffTime -> FilePath -> [(String, String)] -> Text -> IO (Maybe Text)
addPythonDependency timeout dir newEnv depName = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Adding Python dependency " <> depName <> " using uv."
  result <- runProcessWithTimeout timeout "." newEnv "uv" ["add", T.unpack depName]
  eitherToMaybe <$> handleExitCode ("'uv add " <> depName <> "'") result

minimisePythonFileIO :: FilePath -> FilePath -> [(String, String)] -> IO (Either Text Text)
minimisePythonFileIO baseDir path env = Dir.withCurrentDirectory baseDir $ do
  let timeout = 5
  res <- runProcessWithTimeout timeout "." env "pyfile_summariser" [path]
  case res of
    Left err -> pure . Left $ "Error minimising Python file " <> T.pack path <> ": " <> err
    Right (exitCode, stdoutRes, stderrRes) ->
      case exitCode of
        Exit.ExitSuccess -> pure $ Right stdoutRes
        Exit.ExitFailure code ->
          pure
            . Left
            $ "Failed to minimise python file "
            <> T.pack path
            <> " with exit code "
            <> show code
            <> "\nstdout:\n"
            <> truncateText 40 stdoutRes
            <> "\nstderr:\n"
            <> truncateText 40 stderrRes


minimisePythonFileContents :: Text -> AppM (Either Text Text)
minimisePythonFileContents pathText = do
  cfg <- ask
  envVars <- getEnvVars
  unless (isPythonFileExtension pathText) $
    throwError $ "Error: can only process Python source files (.py, .pyi) for 'minimisation', not " <> pathText
  minimiserExists <- liftIO $ checkBinaryOnPath "gofile_summariser" envVars
  unless minimiserExists $ throwError "Error: missing pyfile_summariser binary on path; need to install https://github.com/outervation/pyfile_summariser"
  let baseDir = configBaseDir cfg
      filePath = toFilePath cfg pathText
  res <- liftIO $ minimisePythonFileIO baseDir filePath envVars
  case res of
    Left err -> pure $ Left $ "Error minimising python file: " <> err
    Right txt -> pure $ Right txt

isPythonTestFile :: Text -> AppM Bool
isPythonTestFile fileName = pure $
  if not (isPythonFileExtension fileName)
    then False
    else
      let nameOnly = FP.takeFileName (T.unpack fileName)
          -- Check for "tests" or "test" in the directory path components
          isInTestDir = let dirs = FP.splitDirectories (FP.takeDirectory (T.unpack fileName))
                        in "tests" `elem` dirs || "test" `elem` dirs
      in ("test_" `L.isPrefixOf` nameOnly) || ("_test.py" `L.isSuffixOf` nameOnly) || isInTestDir

addPythonStyleLineNumberComment :: Int -> Text -> Text
addPythonStyleLineNumberComment idx originalLine =
  let comment = "## " <> T.pack (show idx) <> " ##"
   in if T.null originalLine
        then comment
        else originalLine <> " " <> comment 

removePythonStyleLineNumberComment :: Text -> Text
removePythonStyleLineNumberComment line =
  -- 1. Check for the outermost " ##" suffix.
  case T.stripSuffix " ##" line of
    Nothing -> line -- Not our comment style if it doesn't end with " ##"
    Just lineWithoutOuterSuffix ->
      -- lineWithoutOuterSuffix is now "potential_content ## number" or "## number"
      -- 2. Find the last "## " that separates the content/marker from the number.
      -- T.breakOnEnd splits *after* the separator. If separator not found, first part is empty.
      let (prefixCandidate, numberCandidate) = T.breakOnEnd "## " lineWithoutOuterSuffix
      in -- 3. Validate the parts:
         --    - prefixCandidate must not be empty (it should contain at least "## ")
         --    - numberCandidate must not be empty and must be all digits.
         if T.null prefixCandidate || T.null numberCandidate || not (T.all isDigit numberCandidate)
           then line -- Doesn't match the "content_or_empty ## digits" structure
           else
             -- At this point, we have:
             -- prefixCandidate = "original_content_possibly_with_space_suffix ## " (e.g., "text ## " or "## ")
             -- numberCandidate = "digits" (e.g., "99")
             --
             -- 4. Get the part before "## "
             let contentBeforeCommentMarker = T.dropEnd (T.length "## ") prefixCandidate
             in
               -- 5. If addPythonStyleLineNumberComment added a space before "## ", remove it.
               --    If the original line was empty, contentBeforeCommentMarker will be "".
               --    T.stripSuffix " " "" is Nothing. In this case, we want contentBeforeCommentMarker ("").
               --    If contentBeforeCommentMarker was "text ", T.stripSuffix gives Just "text".
               fromMaybe contentBeforeCommentMarker (T.stripSuffix " " contentBeforeCommentMarker)

removeCommentTest :: IO ()
removeCommentTest = do
  let testCases =
        [ ("Simple line", "Hello world", "Hello world") -- No comment
        , ("Comment only", addPythonStyleLineNumberComment 1 "", "")
        , ("Line with comment", addPythonStyleLineNumberComment 42 "Some text", "Some text")
        , ("Empty line input", "", "")
        , ("Line with numbers", addPythonStyleLineNumberComment 123 "Line 123", "Line 123")
        , ("Malformed comment (no space)", "Text##1##", "Text##1##")
        , ("Malformed comment (no end)", "Text ## 1 #", "Text ## 1 #")
        , ("Malformed comment (not number)", "Text ## abc ##", "Text ## abc ##")
        , ("Comment-like text not at end", "## 1 ## Some text", "## 1 ## Some text")
        , ("Just the comment start", "## 5", "## 5")
        , ("Comment start with space", "## 5 ", "## 5 ") -- " ##" not found
        , ("Ends with comment marker but no number", "Text ##  ##", "Text ##  ##") -- numberCandidate would be empty
        , ("Looks like a comment but number has char", "Text ## 1a ##", "Text ## 1a ##")
        , ("Line with leading/trailing spaces around valid comment",
           addPythonStyleLineNumberComment 7 "  leading spaces and trailing  ",
           "  leading spaces and trailing  ")
        , ("Line that is just spaces", "   ", "   ")
        , ("Line that becomes empty after stripping comment", addPythonStyleLineNumberComment 99 "", "")
        ]

  mapM_ (\(desc, input, expected) -> do
    let output = removePythonStyleLineNumberComment input
    putStrLn $ desc ++ ":"
    putStrLn $ "  Input   : " ++ show input
    putStrLn $ "  Output  : " ++ show output
    putStrLn $ "  Expected: " ++ show expected
    if output == expected
      then putStrLn "  Result  : PASS"
      else putStrLn $ "  Result  : FAIL (Expected " ++ show expected ++ ", Got " ++ show output ++ ")"
    putStrLn "") testCases

  -- Test specific interaction
  let original1 = T.pack "This is a test line."
  let commented1 = addPythonStyleLineNumberComment 10 original1
  let uncommented1 = removePythonStyleLineNumberComment commented1
  putStrLn "Interaction test 1:"
  putStrLn $ "  Original:     " ++ show original1
  putStrLn $ "  Commented:    " ++ show commented1
  putStrLn $ "  Uncommented:  " ++ show uncommented1
  putStrLn $ "  Matches original? " ++ show (original1 == uncommented1)
  putStrLn ""

  let original2 = T.empty
  let commented2 = addPythonStyleLineNumberComment 20 original2
  let uncommented2 = removePythonStyleLineNumberComment commented2
  putStrLn "Interaction test 2 (empty original):"
  putStrLn $ "  Original:     " ++ show original2
  putStrLn $ "  Commented:    " ++ show commented2
  putStrLn $ "  Uncommented:  " ++ show uncommented2
  putStrLn $ "  Matches original? " ++ show (original2 == uncommented2)
  putStrLn ""

data Python = Python

instance BuildSystem Python where
  buildProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ buildProjectPython timeout baseDir envVars

  testProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ runTestsPython timeout baseDir envVars

  setupProject cfg projectCfg = liftIO $ setupDirectoryPython cfg projectCfg

  isBuildableFile fileName = pure $ isPythonFileExtension fileName

  isTestFile = isPythonTestFile

  getIgnoredDirs = pure [
      ".venv", 
      "__pycache__", 
      ".pytest_cache", 
      "dist", 
      "build", 
      ".eggs", 
      "*.egg-info", 
      ".hypothesis", 
      ".git", 
      "htmlcov", -- coverage report
      ".mypy_cache",
      ".ruff_cache"
      ]

  getFormatChecker cfg = do
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    pure $ checkFormatPython timeout baseDir

  minimiseFile = minimisePythonFileContents

  addDependency name = do
    cfg <- ask
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ addPythonDependency timeout baseDir envVars name

  addLineNumberComment num txt = addPythonStyleLineNumberComment num txt
  removeLineNumberCommentIfPresent txt = removePythonStyleLineNumberComment txt
