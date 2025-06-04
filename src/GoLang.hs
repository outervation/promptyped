{-# LANGUAGE OverloadedStrings #-}

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
import Data.List qualified as L
import System.Exit qualified as Exit
import System.FilePath qualified as FP

maxTestFailLinesToShowFullOutput :: Int
maxTestFailLinesToShowFullOutput = 400

eitherToMaybe :: Either Text () -> Maybe Text
eitherToMaybe (Left err) = Just err
eitherToMaybe _ = Nothing

-- | Extracts Go test names from lines like "--- FAIL: TestName (duration)"
-- Example input line: "--- FAIL: TestFeatureCursor_T1_Calculation (0.01s)"
-- Example output: "TestFeatureCursor_T1_Calculation"
extractFailedGoTestsSimple :: Text -> [Text]
extractFailedGoTestsSimple output =
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

-- | Extracts Go test names from lines like "--- FAIL: TestName (duration)"
-- | and from timeout reports like:
-- |   panic: test timed out after 10s
-- |           running tests:
-- |                   TestRunApplication_BasicDataFlow (10s)
extractFailedGoTestsTopLevel :: Text -> [Text]
extractFailedGoTestsTopLevel output =
  -- nub to remove duplicates, reverse because foldl' builds the list backwards
  L.nub . reverse . snd $ foldl' processLine (False, []) (T.lines output)
  where
    -- State: (isCurrentlyParsingTimeoutList, accumulatedTests)
    -- isCurrentlyParsingTimeoutList: True if we've seen "panic...running tests:" and expect indented test names.
    processLine :: (Bool, [Text]) -> Text -> (Bool, [Text])
    processLine (inTimeoutList, acc) currentLine =
      let sCurrentLine = T.strip currentLine
          hasLeadingSpace = T.length currentLine > T.length (T.stripStart currentLine)

          -- 1. Accumulate tests from "--- FAIL:" lines. This happens independently.
          accAfterFailCheck =
            case T.stripPrefix "--- FAIL:" sCurrentLine of
              Just restAfterFailMarker ->
                case T.words (T.strip restAfterFailMarker) of
                  (testName : _) -> testName : acc
                  _              -> acc -- Malformed "--- FAIL:"
              Nothing -> acc

          -- 2. Handle timeout logic and determine the next state for inTimeoutList.
          (nextInTimeoutList, finalAcc) =
            if T.pack "panic: test timed out" `T.isInfixOf` currentLine then
              -- This line starts a new timeout sequence.
              -- The accumulator (accAfterFailCheck) already includes any "--- FAIL:" from this exact line (unlikely).
              (True, accAfterFailCheck)
            else if inTimeoutList then
              -- We are currently in a timeout sequence (because a previous line was a "panic").
              if hasLeadingSpace then
                -- This line is indented. It might be "running tests:" or a timed-out test name.
                if T.pack "running tests:" == sCurrentLine then
                  (True, accAfterFailCheck) -- Confirmed "running tests:", stay in timeout list.
                else if not (T.null sCurrentLine) && -- Ensure not an empty stripped line
                         T.last sCurrentLine == ')' &&
                         T.isInfixOf (T.pack "(") sCurrentLine &&
                         not (T.pack "--- FAIL:" `T.isInfixOf` sCurrentLine) then -- Make sure it's not a --- FAIL line
                  -- This looks like an indented test name, e.g., "TestName (duration)"
                  case T.splitOn (T.pack "(") sCurrentLine of
                    (namePart : _) ->
                      let testName = T.strip namePart
                      in if not (T.null testName) then
                           (True, testName : accAfterFailCheck) -- Add timed-out test, stay in list.
                         else
                           (True, accAfterFailCheck) -- Empty name part, stay in list.
                    _ -> (True, accAfterFailCheck) -- No '(', stay in list (should be caught by isInfixOf).
                else
                  -- Indented line, but not "running tests:" or a recognized test name format.
                  -- Assume it's other log output within the timeout block; stay in the timeout list.
                  (True, accAfterFailCheck)
              else
                -- Unindented line while inTimeoutList: this terminates the current timeout test list.
                (False, accAfterFailCheck)
            else
              -- Not a "panic" line, and not currently in a timeout list.
              (False, accAfterFailCheck)

      in (nextInTimeoutList, finalAcc)

escapeRegexBrackets :: Text -> Text
escapeRegexBrackets = T.concatMap escapeChar
  where
    escapeChar :: Char -> Text
    escapeChar '(' = T.pack "\\("
    escapeChar ')' = T.pack "\\)"
    escapeChar '[' = T.pack "\\["
    escapeChar ']' = T.pack "\\]"
    escapeChar c   = T.singleton c

-- | Extracts Go test names from lines like "--- FAIL: TestName (duration)"
-- | and from timeout reports like:
-- |   panic: test timed out after 10s
-- |           running tests:
-- |                   TestRunApplication_BasicDataFlow (10s)
-- | For nested "--- FAIL:" blocks, it extracts only the most deeply nested test name
-- | from each such block.
extractFailedGoTests :: Text -> [Text]
extractFailedGoTests output =
  -- State for foldl':
  --   ( Maybe (Int, Text) -- Current deepest FAIL candidate: (indentation, testName)
  --   , Bool                -- isCurrentlyParsingTimeoutList
  --   , [Text]              -- accumulatedTests (reversed, new tests are prepended)
  --   )
  let initialState = (Nothing, False, [])
      (finalDeepestFail, _finalInTimeoutList, accumulatedTestsReversed) =
        foldl' processLine initialState (T.lines output)

      -- After processing all lines, if there's a pending deepest FAIL test candidate
      -- (i.e., the last test block in the input was a FAIL block), add it to the results.
      finalAccumulatedTests =
        case finalDeepestFail of
          Just (_, testName) -> testName : accumulatedTestsReversed
          Nothing            -> accumulatedTestsReversed
  in
  -- nub to remove duplicates (e.g., if a test is both a FAIL and timed out),
  -- and reverse to restore original encounter order (though order isn't strictly guaranteed
  -- between different types of failures or deeply nested items).
  L.nub . reverse $ finalAccumulatedTests

processLine :: (Maybe (Int, Text), Bool, [Text]) -> Text -> (Maybe (Int, Text), Bool, [Text])
processLine (mCurrentDeepestFail, inTimeoutList, acc) currentLine =
  let sCurrentLine = T.strip currentLine -- Stripped line for content checks (e.g., markers)
      -- Indentation is calculated from the original line to preserve formatting info
      currentIndent = T.length currentLine - T.length (T.stripStart currentLine)
      failMarker = T.pack "--- FAIL:"
      isCurrentFailLine = failMarker `T.isPrefixOf` sCurrentLine

      -- --- Handling of "--- FAIL:" lines (Nested Fail Logic) ---

      -- Determine if the mCurrentDeepestFail (from previous lines) should be flushed.
      -- A pending deepest FAIL test is flushed (i.e., added to 'acc') if:
      -- 1. The current line is a new "--- FAIL:" line at an equal or lesser indent
      --    than the pending one. This means the previous nested block ended.
      -- 2. The current line is NOT a "--- FAIL:" line, is not empty/whitespace-only,
      --    and is less indented than the pending FAIL line's declaration. This signals
      --    the end of that FAIL block's associated log lines.
      -- 3. The current line indicates a "panic: test timed out". This special event also
      --    finalizes any pending FAIL block before handling the timeout itself.
      (accAfterPotentialFlush, mEffectiveDeepestFail) =
        case mCurrentDeepestFail of
          Just (prevFailIndent, prevFailTestName) ->
            -- Check for panic on the raw currentLine as it might not be at the start after stripping.
            let isPanicLineForFlush = not isCurrentFailLine && T.pack "panic: test timed out" `T.isInfixOf` currentLine
                shouldFlush =
                  (isCurrentFailLine && currentIndent <= prevFailIndent) ||
                  (not isCurrentFailLine && not (T.null sCurrentLine) && currentIndent < prevFailIndent) ||
                  isPanicLineForFlush
            in if shouldFlush
                 then (prevFailTestName : acc, Nothing) -- Flushed: add to acc, reset current deepest for this cycle.
                 else (acc, mCurrentDeepestFail)       -- Not flushed: carry over acc and current deepest.
          Nothing -> (acc, Nothing) -- Nothing to flush, acc and current deepest (Nothing) carry over.

      -- Update the deepest FAIL candidate if the current line *is* a "--- FAIL:" line.
      -- It uses mEffectiveDeepestFail, which is mCurrentDeepestFail or Nothing if it was just flushed.
      -- If it's a FAIL line, it becomes the new candidate, replacing any mEffectiveDeepestFail.
      mNextDeepestFail =
        if isCurrentFailLine then
          case T.words (T.strip (T.drop (T.length failMarker) sCurrentLine)) of
            (testName : _) -> Just (currentIndent, testName) -- This FAIL line becomes the new candidate.
            _              -> mEffectiveDeepestFail         -- Malformed "--- FAIL:", carry over effective candidate.
        else
          mEffectiveDeepestFail -- Not a "--- FAIL:" line, carry over effective candidate from flush step.

      -- --- Handling of Timeout Reports ---
      -- Timeout test names are added directly to the accumulator (accAfterPotentialFlush).
      (nextInTimeoutList, finalAcc) =
        let hasLeadingSpace = T.length currentLine > T.length (T.stripStart currentLine)
            isPanicLine = T.pack "panic: test timed out" `T.isInfixOf` currentLine -- Check on raw currentLine
        in if isPanicLine then
             -- A panic line starts a new timeout sequence.
             -- accAfterPotentialFlush already includes any test name flushed due to this panic line.
             (True, accAfterPotentialFlush)
           else if inTimeoutList then
             -- We are currently in a timeout sequence (because a previous line was a "panic" or "running tests:").
             if hasLeadingSpace then
               -- This line is indented. It might be "running tests:" or a timed-out test name.
               if T.pack "running tests:" == sCurrentLine then
                 (True, accAfterPotentialFlush) -- Confirmed "running tests:", stay in timeout list.
               else if not (T.null sCurrentLine) && -- Ensure not an empty stripped line
                        T.last sCurrentLine == ')' && -- Common pattern: "TestName (duration)"
                        T.isInfixOf (T.pack "(") sCurrentLine &&
                        not isCurrentFailLine then -- Make sure it's not a "--- FAIL:" line (those are handled above)
                 case T.splitOn (T.pack "(") sCurrentLine of
                   (namePart : _) ->
                     let timedOutTestName = T.strip namePart
                     in if not (T.null timedOutTestName) then
                          (True, timedOutTestName : accAfterPotentialFlush) -- Add timed-out test, stay in list.
                        else
                          (True, accAfterPotentialFlush) -- Empty name part after stripping, stay in list.
                   _ -> (True, accAfterPotentialFlush) -- No '(', or malformed; stay in list.
               else
                 -- Indented line, but not "running tests:" or a recognized test name format.
                 -- Assume it's other log output within the timeout block; stay in the timeout list.
                 (True, accAfterPotentialFlush)
             else
               -- Unindented line while inTimeoutList: this terminates the current timeout test list.
               (False, accAfterPotentialFlush)
           else
             -- Not a "panic" line, and not currently in a timeout list.
             (False, accAfterPotentialFlush)

  in (mNextDeepestFail, nextInTimeoutList, finalAcc)

tmpp :: IO [Text]
tmpp = do
  contents <- readFileToText "/home/dfxs/tmpout2.txt"
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
      forM_ (projectInitialFiles projectCfg) $ uncurry TIO.writeFile
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
        --buildResult <- runProcessWithTimeout timeout "." newEnv "go" ["build", "./..."]
        --handleExitCode "'go build'" buildResult
        buildResult <- runProcessWithTimeout timeout "." newEnv "./build_all_go_binaries.sh" []
        handleExitCode "'build_all_go_binaries.sh'" buildResult
      buildTests = do
        let lineToSkip x = "no tests to run" `T.isInfixOf` x || "no test files" `T.isInfixOf` x
            removeNoTestLines txt = unlines $ filter (not . lineToSkip) $ lines txt
        buildTestsResult <- runProcessWithTimeout timeout "." newEnv "go" ["test", "-run=^$", "./..."]
        first removeNoTestLines <$> handleExitCode "'go test -run=^$'" buildTestsResult
      fmtIt = do
        fmtResult <- runProcessWithTimeout timeout "." newEnv "go" ["fmt", "./..."]
        handleExitCode "'go fmt'" fmtResult
      generateIt = do
        generateResult <- runProcessWithTimeout timeout "." newEnv "go" ["generate", "./..."]
        handleExitCode "'go generate'" generateResult
  eitherToMaybe <$> runAll [generateIt, buildIt, buildTests, fmtIt]

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
  testResult <- runProcessWithTimeout timeout "." newEnv "go" ["test", "-timeout", "40s", "./..."]
  eitherToMaybe <$> handleExitCode "'go test'" testResult

runTestsGo ::
  NominalDiffTime ->
  FilePath ->
  [(String, String)] ->
  IO (Maybe (Text, NumFailedTests))
runTestsGo timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Running initial Go tests in dir " <> toText dir
  let initialArgs = ["test", "-timeout", "40s", "./..."]
  -- initialTestResult :: IO (Either Text (Exit.ExitCode, Text, Text))
  initialTestResult <- runProcessWithTimeout timeout "." newEnv "go" initialArgs

  case initialTestResult of
    Left procErrText -> do
      -- This error is from runProcessWithTimeout itself (e.g., timeout, command not found)
      let errMsg = "Initial Go tests ('go test ./...') execution failed: " <> procErrText
      putTextLn errMsg
      pure $ Just (errMsg, -1)

    Right (initialExitCode, initialStdout, initialStderr) ->
      case initialExitCode of
        Exit.ExitSuccess -> do
          putTextLn "Initial Go tests passed."
          pure Nothing

        Exit.ExitFailure _ -> do
          let combinedOutput = initialStdout <> "\n" <> initialStderr
              totalLines = length $ T.lines combinedOutput
              opNameInitial = "'go test ./...'"
              -- sort in descending order of length so nested tests are picked first
              failedTests = sortOn (Down . T.length) $ L.nub $ extractFailedGoTests combinedOutput
              numFailedTests = length failedTests


          if totalLines <= maxTestFailLinesToShowFullOutput then do
            putTextLn "Initial Go tests failed. Output is small, reporting directly."
            -- Pass the Right part of the result (successful execution, but failed test)
            -- to handleExitCode.
            eitherRes <- handleExitCode opNameInitial (Right (initialExitCode, initialStdout, initialStderr))
            let res = eitherToMaybe eitherRes
                res' = (\x -> (x, NumFailedTests numFailedTests)) <$> res
            pure res'

          else do
            putTextLn $ "Initial Go tests failed. Output has " <> T.pack (show totalLines) <> " lines (" <> T.pack (show maxTestFailLinesToShowFullOutput) <> " max for full). Attempting to extract and re-run."
            case failedTests of
              [] -> do
                let err = "Error: Unit tests failed but could not extract specific failed test names. It's likely a unit tests is somehow corrupting/overriding the output of go test. Please remove any tests that overwrite the stdout/stderr!"
                putTextLn $ err
                pure $ Just (err, NumFailedTests (-1))
                -- ioError (userError err)
                -- eitherToMaybe <$> handleExitCode opNameInitial (Right (initialExitCode, initialStdout, initialStderr))

{-
              [_oneTestFromExtraction] -> do
                 -- Even if one test is extracted, if the initial failure was large,
                 -- re-running just one might hide interactions. Report original.
                 putTextLn $ "Initial Go tests failed (large output), and only one specific test name (" <> _oneTestFromExtraction <> ") was extracted. Reporting the original full failure as it's more informative than a potentially successful single re-run or a single re-run failure out of its original context."
                 eitherToMaybe <$> handleExitCode opNameInitial (Right (initialExitCode, initialStdout, initialStderr))
-}
              multipleFailingTests -> do
                putTextLn $ "Extracted " <> T.pack (show $ length multipleFailingTests) <> " potentially failing tests: " <> T.intercalate ", " multipleFailingTests <> ". Attempting to re-run them one by one."
                res <- tryRerunOneByOne timeout newEnv multipleFailingTests
                case res of
                  Just err -> pure $ Just ("Encountered " <> show numFailedTests <> " test failures: " <> show multipleFailingTests <> ", reran just a single test (for fixing tests one by one) and got output: " <> err, NumFailedTests numFailedTests)
                  Nothing -> pure Nothing

-- Helper function to try re-running tests one by one
tryRerunOneByOne ::
  NominalDiffTime ->
  [(String, String)] ->
  [Text] -> -- List of test names to try
  IO (Maybe Text) -- Just errorText if a test fails or process error,
                  -- or specific message if all pass individually.
                  -- Nothing should not be returned by this helper if called with non-empty list.
tryRerunOneByOne _ _ [] = do
  -- This case is reached if multipleFailingTests was not empty initially,
  -- but all of them passed when re-run individually.
  let msg = "Initial tests failed, but all extracted failing tests passed when re-run individually. This suggests a test interaction issue, a problem not reproducible with a single test re-run, or that the extracted test names were not the sole culprits of the original large failure."
  putTextLn msg
  pure $ Just msg

tryRerunOneByOne currentTimeout currentNewEnv (testNameToRun : restTestsToTry) = do
  -- Go's -run flag takes a regex. Wrap with ^ and $ for exact matches.
  --let runRegex = "^(" <> testNameToRun <> ")$"
  -- We just use the full name, as the regex fails to match nested tests
  let runRegex = escapeRegexBrackets testNameToRun
  -- Note: T.unpack is important for [String] args
  let rerunArgs = ["test", "-run", T.unpack runRegex, "-timeout", "10s", "./..."]
  let opNameRerun = "'go test -run=" <> runRegex <> "'"

  putTextLn $ "Attempting to re-run individually: go " <> T.unwords (map T.pack rerunArgs)
  -- rerunResultOrError :: Either Text (Exit.ExitCode, Text, Text)
  rerunResultOrError <- runProcessWithTimeout currentTimeout "." currentNewEnv "go" rerunArgs

  case rerunResultOrError of
    Left rerunProcErrText -> do
      -- This is an error from runProcessWithTimeout itself for the re-run attempt
      let errMsg = "Error (process execution) occurred trying to re-run test " <> testNameToRun <> ": " <> rerunProcErrText
      putTextLn errMsg
      pure $ Just errMsg -- Report this process-level error

    Right (rerunExitCode, rerunStdout, rerunStderr) -> do
      -- The process ran. Now use handleExitCode to check if the *test itself* failed.
      -- We pass the Right part of rerunResultOrError to handleExitCode.
      maybeTestFailureErr <- eitherToMaybe <$> handleExitCode opNameRerun (Right (rerunExitCode, rerunStdout, rerunStderr))

      case maybeTestFailureErr of
        Just errMessageFromHandleExitCode -> do -- This specific test failed when run in isolation
          putTextLn $ "Test " <> testNameToRun <> " failed when re-run individually. Reporting this failure."
          pure $ Just errMessageFromHandleExitCode -- Return the error of the first test that fails in isolation
        Nothing -> do -- This specific test passed when run in isolation
          putTextLn $ "Test " <> testNameToRun <> " passed when re-run individually. Trying next."
          tryRerunOneByOne currentTimeout currentNewEnv restTestsToTry -- Recurse with remaining tests

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
    Left err -> pure . Left $ "Error minimising Go file " <> T.pack path <> ": " <> err
    Right (exitCode, stdoutRes, stderrRes) ->
      case exitCode of
        Exit.ExitSuccess -> pure $ Right stdoutRes
        Exit.ExitFailure code ->
          pure
            . Left
            $ "Failed to minimise go file "
            <> T.pack path
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
  unless minimiserExists $ throwError "Error: missing gofile_summariser binary on path; need to go install https://github.com/outervation/gofile_summariser"
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

  isTestFile fileName = pure $ isGoFileExtension fileName && "_test.go" `T.isSuffixOf` fileName

  getIgnoredDirs = pure ["build", ".git", "contrib", ".venv"]

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

  addLineNumberComment num txt = addCppStyleLineNumberComment num txt
  removeLineNumberCommentIfPresent txt = removeCppStyleLineNumberComment txt
