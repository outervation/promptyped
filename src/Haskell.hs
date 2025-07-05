{-# LANGUAGE OverloadedStrings #-}

module Haskell where

import BuildSystem
import Control.Monad.Except (throwError)
import Control.Exception (try)
import Core
import Data.Char (isDigit)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import FileSystem (checkBinaryOnPath, gitInit, gitSetupUser, handleExitCode, runAll, runProcessWithTimeout, toFilePath)
import Relude hiding (unwords)
import System.Directory qualified as Dir
import System.Exit qualified as Exit
import System.FilePath qualified as FP

maxTestFailLinesToShowFullOutput :: Int
maxTestFailLinesToShowFullOutput = 400

eitherToMaybe :: Either Text a -> Maybe Text
eitherToMaybe (Left err) = Just err
eitherToMaybe _ = Nothing

isHaskellFileExtension :: Text -> Bool
isHaskellFileExtension fileName = ".hs" `T.isSuffixOf` fileName || ".lhs" `T.isSuffixOf` fileName

-- | Custom line number comment style for Haskell
addHaskellStyleLineNumberComment :: Int -> Text -> Text
addHaskellStyleLineNumberComment idx originalLine =
  let comment = "-- " <> T.pack (show idx) <> " --"
   in if T.null originalLine
        then comment
        else comment <> " " <> originalLine

-- | Custom line number comment remover for Haskell
removeHaskellStyleLineNumberComment :: Text -> Text
removeHaskellStyleLineNumberComment line =
  let (leadingSpaces, afterIndent) = T.span (== ' ') line
   in case T.stripPrefix "-- " afterIndent of
        Nothing -> line -- Does not start with our comment format
        Just afterOpen ->
          let (digitsPart, afterDigits) = T.breakOn " --" afterOpen
           in if T.all isDigit (T.strip digitsPart)
                then case T.stripPrefix " --" afterDigits of
                       Nothing -> line -- Not a proper comment, no closing "--"
                       Just afterClose ->
                         -- Remove the comment plus one space after it, if present.
                         fromMaybe afterClose (T.stripPrefix " " afterClose)
                else line -- Content between markers wasn't digits
                
-- | Extracts Hspec test names from `cabal test` output.
-- It looks for lines in the "Failures:" block that start with a number, e.g., "  1) Some.Module ..."
extractFailedHspecTests :: Text -> [Text]
extractFailedHspecTests output =
  L.nub . reverse . snd $ foldl' processLine (False, []) (T.lines output)
  where
    processLine :: (Bool, [Text]) -> Text -> (Bool, [Text])
    processLine (inBlock, acc) line
      | T.strip line == "Failures:" = (True, acc)
      | inBlock =
          if T.isPrefixOf "  " line
            then case T.words (T.stripStart line) of
              (numWord : testDescParts)
                | T.isSuffixOf ")" numWord && T.all isDigit (T.init numWord) ->
                    -- This is a test failure line, like "1) module, behavior"
                    (True, T.unwords testDescParts : acc)
              _ -> (True, acc) -- An indented line, but not a test summary (e.g., "expected: ...")
            else
              if not (T.null (T.strip line))
                then (False, acc) -- Unindented non-empty line ends the block
                else (True, acc) -- Keep going on blank lines within the block
      | otherwise = (False, acc)

setupDirectoryHaskell :: Config -> ProjectConfig -> IO (Maybe Text)
setupDirectoryHaskell cfg projectCfg = do
  Dir.createDirectoryIfMissing True projectDir
  Dir.withCurrentDirectory projectDir $ do
    let cabalFile = projectDir FP.</> (FP.takeBaseName projectDir ++ ".cabal")
    cabalFileExists <- Dir.doesFileExist cabalFile
    let timeout = secondsToNominalDiffTime 120

    eitherToMaybe
      <$> runAll
        [ initializeIfNeeded cabalFileExists timeout,
          addDeps timeout,
          addInitialTestFile,
          addDocs,
          gitInit projectDir,
          gitSetupUser cfg
        ]
  where
    projectDir = configBaseDir cfg

    initializeIfNeeded :: Bool -> NominalDiffTime -> IO (Either Text ())
    initializeIfNeeded True _ = pure (Right ())
    initializeIfNeeded False time = do
      let initArgs = ["init", "--non-interactive", "--minimal", "--lib", "--source-dir=src", "--test-dir=test"]
      eRes <- runProcessWithTimeout time "." [] "cabal" initArgs
      handleExitCode "'cabal init'" eRes

    addDeps :: NominalDiffTime -> IO (Either Text ())
    addDeps time = do
      cabalEditExists <- checkBinaryOnPath "cabal-edit" []
      return $ case cabalEditExists of
        True -> Right ()
        False -> Left "Error: `cabal-edit` is not on the PATH. Please install it (`cabal install cabal-edit`) to manage Haskell dependencies."

      -- Add hspec to test dependencies
      let addHspec = do
            eRes <- runProcessWithTimeout time "." [] "cabal-edit" ["add-dependency", "hspec", "--test"]
            handleExitCode "'cabal-edit add-dependency hspec --test'" eRes
      
      -- Add project dependencies to the library
      let depNames = projectDependencyNames projectCfg
      let addProjectDeps = runAll $ flip map depNames $ \depName -> do
            eRes <- runProcessWithTimeout time "." [] "cabal-edit" ["add-dependency", T.unpack depName, "--library"]
            handleExitCode ("'cabal-edit add-dependency " <> depName <> " --library'") eRes
      
      addHspec >> addProjectDeps

    addInitialTestFile :: IO (Either Text ())
    addInitialTestFile = do
      let testDir = projectDir FP.</> "test"
      Dir.createDirectoryIfMissing True testDir
      TIO.writeFile (testDir FP.</> "Spec.hs") $
        T.unlines
          [ "{-# LANGUAGE OverloadedStrings #-}",
            "import Test.Hspec",
            "main :: IO ()",
            "main = hspec $ do",
            "  describe \"Initial Test\" $ do",
            "    it \"should pass\" $ do",
            "      True `shouldBe` True"
          ]
      return $ Right ()

    addDocs :: IO (Either Text ())
    addDocs = do
      forM_ (projectInitialFiles projectCfg) $ \(path, content) -> do
        let fullPath = projectDir FP.</> path
        Dir.createDirectoryIfMissing True (FP.takeDirectory fullPath)
        TIO.writeFile fullPath content
      pure $ Right ()

checkFormatHaskell :: NominalDiffTime -> FilePath -> IO (Maybe Text)
checkFormatHaskell timeout dir = do
  ormoluExists <- checkBinaryOnPath "ormolu" []
  case ormoluExists  of
    False -> return $ Just "Error: `ormolu` is not on the PATH. Please install it (`cabal install ormolu`) to check Haskell code formatting."
    True -> Dir.withCurrentDirectory dir $ do
      -- Ormolu checks files in place, so we check src and test dirs.
      let formatArgs = ["--mode", "check", "src", "test"]
      formatResult <- runProcessWithTimeout timeout "." [] "ormolu" formatArgs
      res <- handleExitCode "'ormolu --mode check'" formatResult
      case res of
        Left err -> pure . Just $ "The change you attempted to make would produce invalid syntax. 'ormolu' failed with:\n" <> err <> "\nThe change has been rejected. Please ensure your edits result in valid Haskell code."
        Right () -> pure Nothing

buildProjectHaskell :: NominalDiffTime -> FilePath -> [(String, String)] -> IO (Maybe Text)
buildProjectHaskell timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Building Haskell project in dir " <> toText dir
  let buildIt = do
        buildResult <- runProcessWithTimeout timeout "." newEnv "cabal" ["build", "all", "-fno-code"]
        handleExitCode "'cabal build all -fno-code'" buildResult
  eitherToMaybe <$> buildIt

runTestsHaskell :: NominalDiffTime -> FilePath -> [(String, String)] -> IO (Maybe (Text, NumFailedTests))
runTestsHaskell timeout dir newEnv = Dir.withCurrentDirectory dir $ do
  putTextLn $ "Running Haskell tests in dir " <> toText dir
  let initialArgs = ["test", "all"]
  initialTestResult <- runProcessWithTimeout timeout "." newEnv "cabal" initialArgs

  case initialTestResult of
    Left procErrText -> do
      let errMsg = "Cabal test execution ('cabal test all') failed: " <> procErrText
      putTextLn errMsg
      pure $ Just (errMsg, NumFailedTests (-1))
    Right (initialExitCode, initialStdout, initialStderr) ->
      case initialExitCode of
        Exit.ExitSuccess -> do
          putTextLn "Haskell tests passed."
          pure Nothing
        Exit.ExitFailure _ -> do
          let combinedOutput = initialStdout <> "\n" <> initialStderr
              totalLines = length $ T.lines combinedOutput
              opNameInitial = "'cabal test all'"
              failedTests = extractFailedHspecTests combinedOutput
              numFailedTests = length failedTests
          
          if totalLines <= maxTestFailLinesToShowFullOutput || null failedTests
            then do
              putTextLn "Haskell tests failed. Output is small or no specific tests extracted, reporting directly."
              eitherRes <- handleExitCode opNameInitial (Right (initialExitCode, initialStdout, initialStderr))
              let res = eitherToMaybe eitherRes
              pure $ (\x -> (x, NumFailedTests numFailedTests)) <$> res
            else do
              putTextLn $ "Haskell tests failed. Extracted " <> T.pack (show numFailedTests) <> " failed tests. Attempting to re-run them one by one."
              tryRerunHaskellOneByOne timeout newEnv failedTests combinedOutput numFailedTests

tryRerunHaskellOneByOne :: NominalDiffTime -> [(String, String)] -> [Text] -> Text -> Int -> IO (Maybe (Text, NumFailedTests))
tryRerunHaskellOneByOne _ _ [] originalOutput numOriginalFailures = do
  let msg = "Initial tests failed with " <> T.pack (show numOriginalFailures) <> " distinct test failures, but all extracted failing tests passed when re-run individually. This suggests a test interaction issue or a flaky test. Original combined output:\n" <> truncateText 100 originalOutput
  putTextLn msg
  pure $ Just (msg, NumFailedTests numOriginalFailures)
tryRerunHaskellOneByOne timeout newEnv (testNameToRun : rest) originalOutput numOriginalFailures = do
  let rerunArgs = ["test", "all", "--test-options=--match", "--test-options=" <> T.unpack testNameToRun]
  let opNameRerun = "'cabal test all --test-options=--match=\"" <> testNameToRun <> "\"'"

  putTextLn $ "Attempting to re-run individually: " <> T.unwords (map toText rerunArgs)
  rerunResultOrError <- runProcessWithTimeout timeout "." newEnv "cabal" rerunArgs

  case rerunResultOrError of
    Left rerunProcErrText -> do
      let errMsg = "Error (process execution) occurred trying to re-run test \"" <> testNameToRun <> "\": " <> rerunProcErrText
      putTextLn errMsg
      pure $ Just (errMsg, NumFailedTests numOriginalFailures)
    Right (rerunExitCode, rerunStdout, rerunStderr) -> do
      maybeTestFailureErr <- eitherToMaybe <$> handleExitCode opNameRerun (Right (rerunExitCode, rerunStdout, rerunStderr))
      case maybeTestFailureErr of
        Just errMessage -> do
          putTextLn $ "Test \"" <> testNameToRun <> "\" failed when re-run individually. Reporting this failure."
          pure $ Just ("Re-running \"" <> testNameToRun <> "\" (one of " <> T.pack (show numOriginalFailures) <> " original failures) resulted in:\n" <> errMessage, NumFailedTests numOriginalFailures)
        Nothing -> do
          putTextLn $ "Test \"" <> testNameToRun <> "\" passed when re-run individually. Trying next."
          tryRerunHaskellOneByOne timeout newEnv rest originalOutput numOriginalFailures

minimiseHaskellFileIO :: FilePath -> FilePath -> [(String, String)] -> IO (Either Text Text)
minimiseHaskellFileIO baseDir path env = Dir.withCurrentDirectory baseDir $ do
  let timeout = 5
  res <- runProcessWithTimeout timeout "." env "hsfile-summariser" [path]
  case res of
    Left err -> pure . Left $ "Error minimising Haskell file " <> T.pack path <> ": " <> err
    Right (exitCode, stdoutRes, stderrRes) ->
      case exitCode of
        Exit.ExitSuccess -> pure $ Right stdoutRes
        Exit.ExitFailure code ->
          pure
            . Left
            $ "Failed to minimise haskell file "
            <> T.pack path
            <> " with exit code "
            <> show code
            <> "\nstdout:\n"
            <> truncateText 40 stdoutRes
            <> "\nstderr:\n"
            <> truncateText 40 stderrRes

minimiseHaskellFile :: Text -> AppM (Either Text Text)
minimiseHaskellFile pathText = do
  cfg <- ask
  envVars <- getEnvVars
  unless (isHaskellFileExtension pathText) $
    throwError $ "Error: can only minimise Haskell source files (.hs), not " <> pathText
  minimiserExists <- liftIO $ checkBinaryOnPath "hsfile-summariser" envVars
  unless minimiserExists $ throwError "Error: missing hsfile-summariser binary on path. Need to install https://github.com/outervation/hsfile_summariser"
  let baseDir = configBaseDir cfg
      filePath = toFilePath cfg pathText
  res <- liftIO $ minimiseHaskellFileIO baseDir filePath envVars
  case res of
    Left err -> pure $ Left $ "Error minimising haskell file: " <> err
    Right txt -> pure $ Right txt

addHaskellDependency :: NominalDiffTime -> FilePath -> [(String, String)] -> Text -> IO (Maybe Text)
addHaskellDependency timeout dir newEnv depName = do
  cabalEditExists <- checkBinaryOnPath "cabal-edit" newEnv
  return $ case cabalEditExists of 
    False -> Just "Error: `cabal-edit` is not on the PATH. Please install it (`cabal install cabal-edit`) to manage Haskell dependencies."
    True -> Nothing
  
  Dir.withCurrentDirectory dir $ do
    putTextLn $ "Adding Haskell dependency " <> depName
    -- By default, add to the library component.
    let addArgs = ["add-dependency", T.unpack depName, "--library"]
    result <- runProcessWithTimeout timeout "." newEnv "cabal-edit" addArgs
    eitherToMaybe <$> handleExitCode ("'cabal-edit add-dependency " <> depName <> "'") result

data Haskell = Haskell

instance BuildSystem Haskell where
  buildProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ buildProjectHaskell timeout baseDir envVars

  testProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ runTestsHaskell timeout baseDir envVars

  setupProject cfg projectCfg = liftIO $ setupDirectoryHaskell cfg projectCfg

  isBuildableFile fileName = pure $ isHaskellFileExtension fileName

  isTestFile fileName = pure $ isHaskellFileExtension fileName && ("Spec.hs" `T.isSuffixOf` fileName || "test/" `T.isPrefixOf` fileName)

  getIgnoredDirs = pure ["dist-newstyle", ".git", ".cabal-sandbox", "cabal.project.local"]

  getFormatChecker cfg = do
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    pure $ checkFormatHaskell timeout baseDir

  minimiseFile = minimiseHaskellFile

  addDependency name = do
    cfg <- ask
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ addHaskellDependency timeout baseDir envVars name

  addLineNumberComment = addHaskellStyleLineNumberComment
  removeLineNumberCommentIfPresent = removeHaskellStyleLineNumberComment
  
  fileExtension = ".hs"
