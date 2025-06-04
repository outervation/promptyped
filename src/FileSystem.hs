{-# LANGUAGE OverloadedStrings #-}

module FileSystem (replaceInFile, readFileToText, readFileToTextMayThrow, readFileToTextAndOpen, appendToFile, ensureLineNumbers, toFilePath, getFileNames, fileExistsOnDisk, clearFileOnDisk, runProcessWithTimeout, getFileNamesRecursive, handleExitCode, runAll, gitInit, gitAddAndCommit, ensureNoLineNumbers, addTenthLineNumbersToText, updateOpenedFile, reloadOpenFiles, gitSetupUser, gitRevertFile, tryFileOp, checkBinaryOnPath, LineNumberAddRemoveFns) where

import Control.Concurrent.Async (concurrently) 
import Control.Exception (IOException, bracket, try)
import Control.Monad (foldM)
import Control.Monad.Catch qualified as Catch
import Control.Monad.Except (throwError)
import Core
import Data.ByteString qualified as BS
import Data.List as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime)
import Data.Vector qualified as V
import Relude

import System.Directory qualified as DIR
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.FilePath qualified as FP
import System.IO (hClose)
import System.IO.Error (tryIOError)
import System.Process qualified as Proc
import System.Timeout qualified as Timeout

maxErrLinesToShow :: Int
maxErrLinesToShow = 400

runAll :: [IO (Either Text ())] -> IO (Either Text ())
runAll = foldM step (Right ())
  where
    step (Left e) _ = pure (Left e) -- short-circuit
    step (Right ()) action = action

toFilePath :: Config -> Text -> FilePath
toFilePath cfg x = configBaseDir cfg FP.</> T.unpack x

tryFileOp :: LineNumberAddRemoveFns -> FilePath -> (FilePath -> IO (Either T.Text ())) -> AppM (Maybe T.Text) -> Maybe FileChangeBounds -> AppM (Either T.Text ())
tryFileOp lineNumberFns path op checker maybeBounds = do
  let backupPath = path ++ ".bak"
      cleanupBackup = liftIO $ tryIOError $ do
        backupExists <- DIR.doesFileExist backupPath
        when backupExists (DIR.removeFile backupPath)

  Catch.bracket
    (createBackup backupPath)
    ( \backupResult -> case backupResult of
        Right _ -> cleanupBackup
        Left _ -> return $ Right ()
    )
    ( \backupResult -> case backupResult of
        Left err -> return $ Left err
        Right _ -> processOperation backupPath
    )
  where
    -- Create backup and return either error or success
    createBackup :: FilePath -> AppM (Either T.Text ())
    createBackup backupPath = do
      alreadyExists <- liftIO $ DIR.doesFileExist path
      -- Don't create backup if file doesn't already exist
      case alreadyExists of
        False -> return $ Right ()
        True -> do
          result <- liftIO $ tryIOError $ DIR.copyFile path backupPath
          case result of
            Left err -> return $ Left $ T.pack $ "Failed to create backup: " ++ show err
            Right _ -> return $ Right ()

    -- Process the operation and validation
    processOperation :: FilePath -> AppM (Either T.Text ())
    processOperation backupPath = do
      opResult <- liftIO $ op path
      case opResult of
        Left err -> return $ Left err
        Right _ -> validateAndFinalize backupPath

    -- Validate results and handle restoration if needed
    validateAndFinalize :: FilePath -> AppM (Either T.Text ())
    validateAndFinalize backupPath = do
      checkerResult <- checker
      case checkerResult of
        Nothing -> return $ Right ()
        Just err -> restoreBackup backupPath err

    getRelevantFilePart :: Text -> Text
    getRelevantFilePart contents = case maybeBounds of
      Just (FileChangeBounds firstLine lastLine) -> T.unlines $ sliceList firstLine lastLine $ T.lines contents
      Nothing -> contents

    -- Restore from backup (if it exists) when validation fails
    restoreBackup :: FilePath -> T.Text -> AppM (Either T.Text ())
    restoreBackup backupPath err = do
      modifiedFile <- liftIO $ readFileToText path
      let modifiedFileRelevantPart = getRelevantFilePart $ addTenthLineNumbersToText lineNumberFns modifiedFile
      restoreResult <- liftIO $ tryIOError $ do
        DIR.removeFile path
        backupExists <- DIR.doesFileExist backupPath
        when backupExists $ DIR.copyFile backupPath path

      case restoreResult of
        Left restoreErr ->
          return
            $ Left
            $ T.pack
            $ "Validation failed: "
            ++ T.unpack err
            ++ ". Additionally, failed to restore backup: "
            ++ show restoreErr
        Right _ ->
          return . Left $ "The file change you attempted to make was rejected and not applied due to:\n" <> err <> "\n. The relevant part of the file post-modification looked like (note that's not the state of the file on disk, as the change was rejected):\n\n" <> modifiedFileRelevantPart <> "\n\nPlease modify your tool call so the edit doesn't produce syntactically invalid code, and note again that the error above describes what the file would look like post-change, but the change was rejected so the contents of the file on disk are still those seen in the open files section earlier above."

readFileToText :: FilePath -> IO Text
readFileToText path = do
  mayVal <- try @IOException (readFileBS path)
  return $ case mayVal of
    Left _ -> ""
    Right val -> TE.decodeUtf8Lenient val

readFileToTextMayThrow :: FilePath -> IO Text
readFileToTextMayThrow path = do
  val <- readFileBS path
  return $ TE.decodeUtf8Lenient val

updateOpenedFile :: LineNumberAddRemoveFns -> Text -> AppM ()
updateOpenedFile lineNumberFns fileName = do
  theState <- get
  cfg <- ask
  let fmtErr err = "Internal error: failed ensuring no line numbers for " <> fileName <> ": " <> err :: Text
  case fileAlreadyOpen fileName theState of
    False -> throwError $ "Internal error: tried to update non-opened file " <> fileName
    True ->
      when (isNothing $ isFileForbidden cfg fileName)
        $ liftIO (ensureNoLineNumbers lineNumberFns (toFilePath cfg fileName))
        >>= \case
          Left err -> throwError (fmtErr err)
          Right contents -> modify' (updateOpenFile fileName contents)

reloadOpenFiles :: LineNumberAddRemoveFns -> AppM ()
reloadOpenFiles lineNumberFns = do
  st <- get
  let openFiles = map openFileName $ stateOpenFiles st
  forM_ openFiles (updateOpenedFile lineNumberFns)

readFileToTextAndOpen :: FilePath -> IO Text
readFileToTextAndOpen path = do
  exists <- DIR.doesFileExist path
  unless exists $ DIR.createDirectoryIfMissing True (FP.takeDirectory path)
  mayPath <- try @IOException (readFileBS path)
  return $ case mayPath of
    Left _ -> ""
    Right val -> TE.decodeUtf8Lenient val

clearFileOnDisk :: FilePath -> IO ()
clearFileOnDisk path = do
  exists <- DIR.doesFileExist path
  when exists
    $ writeFile path ""

replaceInFile :: FilePath -> Int -> Int -> T.Text -> IO (Either T.Text ())
replaceInFile fileName startLineNumDesired endLineNumDesired newText = do
  putTextLn $ "Editing file " <> show fileName
  -- Try to read the file and handle potential IO exceptions
  fileResult <- try $ TIO.readFile fileName :: IO (Either IOException T.Text)
  case fileResult of
    Left ex -> return $ Left $ T.pack $ "Error reading file: " <> show ex
    Right content -> do
      let theLines = T.lines content
          totalLines = length theLines
          startLineNum = min totalLines startLineNumDesired
          endLineNum = min totalLines endLineNumDesired

      -- Validate input parameters
      case validateReplaceInFileParams totalLines startLineNum endLineNum of
        Left err -> return $ Left err
        Right () -> do
          -- Perform the replacement
          let (before, rest) = splitAt startLineNum theLines
              (_, after) = splitAt (endLineNum - startLineNum) rest
              newLines = before <> T.lines newText <> after
              newContent = T.unlines newLines

          -- Try to write the file and handle potential IO exceptions
          writeResult <- try $ TIO.writeFile fileName newContent :: IO (Either IOException ())
          case writeResult of
            Left ex -> return $ Left $ T.pack $ "Error writing file: " <> show ex
            Right () -> return $ Right ()

validateReplaceInFileParams :: Int -> Int -> Int -> Either T.Text ()
validateReplaceInFileParams totalLines startLineNum endLineNum
  | startLineNum < 0 = Left "startLineNum must be non-negative"
  | endLineNum < 0 = Left "endLineNum must be non-negative"
  | startLineNum >= totalLines = Left $ T.pack $ "startLineNum must be less than total number of lines (" <> show totalLines <> ")"
  | endLineNum > totalLines = Left $ T.pack $ "endLineNum must be less than or equal to total number of lines (" <> show totalLines <> ")"
  | endLineNum < startLineNum = Left $ "endLineNum " <> show endLineNum <> " must be greater than or equal to startLineNum " <> show startLineNum <> ". Note these are internal line numbers, in [startLineNum, endLineNum) format where the end line number is exclusive; this may not exactly match tool input format where the endLineNum is inclusive."
  | otherwise = Right ()

appendToFile :: FilePath -> T.Text -> IO (Either T.Text ())
appendToFile fileName text = do
  -- Try to append to the file and handle potential IO exceptions
  DIR.createDirectoryIfMissing True (FP.takeDirectory fileName)
  appendResult <- try $ TIO.appendFile fileName text :: IO (Either IOException ())
  putTextLn $ "Appending to file " <> show fileName <> ", result is: " <> show appendResult
  case appendResult of
    Left ex -> return $ Left $ T.pack $ "Error appending to file " <> fileName <> ": " <> show ex
    Right () -> return $ Right ()

addTenthLineNumbers :: (Int -> Text -> Text) -> V.Vector Text -> V.Vector Text
addTenthLineNumbers addLineNumberComment = V.imap (addTenthComment addLineNumberComment)

addTenthComment :: (Int -> Text -> Text) -> Int -> Text -> Text
addTenthComment addLineNumberComment idx originalLine =
  if idx `mod` 10 == 0
    then
    addLineNumberComment idx originalLine
    else originalLine

type LineNumberAddRemoveFns = (Int -> Text -> Text, Text -> Text)

ensureNoLineNumbers :: LineNumberAddRemoveFns -> FilePath -> IO (Either Text Text)
ensureNoLineNumbers (_, removeLineNumberComment) filepath = do
  result <- try $ processFileContents filepath :: IO (Either IOException Text)
  pure $ either (Left . T.pack . displayException) Right result
  where
    processFileContents :: FilePath -> IO Text
    processFileContents fp = do
      contents <- readFileToText fp
      let originalLines = V.fromList (T.lines contents)
          processedLines = V.map removeLineNumberComment originalLines
          newContents = T.unlines (V.toList processedLines)
      when (T.length contents > 0) $ TIO.writeFile fp newContents
      pure newContents

addTenthLineNumbersToText :: LineNumberAddRemoveFns -> Text -> Text
addTenthLineNumbersToText (addLineNumberComment, removeLineNumberComment) contents = do
  let originalLines = V.fromList (T.lines contents)
      processedLines = V.map removeLineNumberComment originalLines
      numberedLines = addTenthLineNumbers addLineNumberComment processedLines
      newContents = T.unlines (V.toList numberedLines)
  newContents

ensureLineNumbers :: LineNumberAddRemoveFns -> FilePath -> IO (Either Text Text)
ensureLineNumbers lineNumberFns filepath = do
  result <- try $ processFileContents filepath :: IO (Either IOException Text)
  pure $ either (Left . T.pack . displayException) Right result
  where
    processFileContents :: FilePath -> IO Text
    processFileContents fp = do
      contents <- readFileToText fp
      let newContents = addTenthLineNumbersToText lineNumberFns contents
      when (T.length contents > 0) $ TIO.writeFile fp newContents
      pure newContents

getFileNames :: FilePath -> IO [Text]
getFileNames dir = do
  DIR.createDirectoryIfMissing True dir
  entries <- DIR.listDirectory dir
  pure $ map toText entries

getFileNamesRecursive :: [Text] -> FilePath -> IO [Text]
getFileNamesRecursive foldersToIgnore baseDir = do
  DIR.createDirectoryIfMissing True baseDir
  entries <- DIR.listDirectory baseDir
  let fullPaths = map (baseDir FP.</>) entries
  files <- filterM DIR.doesFileExist fullPaths
  dirs <- filterM DIR.doesDirectoryExist fullPaths
  let allowedDirs = filter ((`Relude.notElem` foldersToIgnore) . toText . FP.takeFileName) dirs

  -- Get relative paths for current directory files
  let relativeFiles = map (toText . FP.makeRelative baseDir) files

  -- For each subdirectory, get files and prepend the subdirectory name to paths
  subDirFiles <- forM allowedDirs $ \dir -> do
    let dirName = toText $ FP.takeFileName dir
    nestedFiles <- getFileNamesRecursive foldersToIgnore dir
    -- Prepend directory name to each nested file
    return $ map (\file -> dirName <> "/" <> file) nestedFiles

  -- Combine current directory files with flattened subdirectory files
  pure $ relativeFiles <> concat subDirFiles

fileExistsOnDisk :: FilePath -> IO Bool
fileExistsOnDisk = DIR.doesFileExist

-- | Run a process and capture its output, with timeout
runProcessWithTimeout ::
  -- | Timeout in seconds
  NominalDiffTime ->
  -- | Working directory
  FilePath ->
  -- | Environment variables
  [(String, String)] ->
  -- | Command
  String ->
  -- | Arguments
  [String] ->
  -- | (exit code, stdout, stderr)
  IO (Either Text (Exit.ExitCode, Text, Text))
runProcessWithTimeout timeout workDir newEnv cmd args = do
  --  dir <- DIR.getCurrentDirectory
  --  putTextLn $ "Running " <> show cmd <> " with args :" <> show args <> " in " <> show dir
  currentEnv <- Env.getEnvironment
  let env = currentEnv ++ newEnv
  let process =
        (Proc.proc cmd args)
          { Proc.cwd = Just workDir,
            Proc.std_out = Proc.CreatePipe,
            Proc.std_err = Proc.CreatePipe,
            Proc.env = Just env
          }
  result <- Timeout.timeout (round $ timeout * 1_000_000) $ do
    bracket (Proc.createProcess process) cleanupProcess $ \case
      (_, Just hout, Just herr, ph) -> do
        (outContent, errContent) <-
          concurrently
            (BS.hGetContents hout)
            (BS.hGetContents herr)
        exitCode <- Proc.waitForProcess ph
        pure $ Right (exitCode, TE.decodeUtf8Lenient outContent, TE.decodeUtf8Lenient errContent)
      _ -> pure $ Left "Failed to create process pipes"

  pure $ fromMaybe (Left "Process timed out") result
  where
    cleanupProcess (_, mbOut, mbErr, ph) = do
      mapM_ hClose mbOut
      mapM_ hClose mbErr
      Proc.terminateProcess ph

handleExitCode :: Text -> Either Text (Exit.ExitCode, Text, Text) -> IO (Either Text ())
handleExitCode opName res = handleExitCodeInner opName (Just maxErrLinesToShow) res
 
handleExitCodeInner :: Text -> Maybe Int -> Either Text (Exit.ExitCode, Text, Text) -> IO (Either Text ())
handleExitCodeInner opName mayMaxLinesToShow res = do
  let maybeTruncateText = case mayMaxLinesToShow of
        Just maxLines -> truncateText maxLines
        Nothing -> id
  case res of
    Left err -> pure $ Left err
    Right (exitCode, stdoutRes, stderrRes) ->
      case exitCode of
        Exit.ExitSuccess -> pure $ Right ()
        Exit.ExitFailure code ->
          pure
            . Left
            $ "Failed to run "
            <> opName
            <> " with exit code "
            <> show code
            <> "\nstdout:\n"
            <> maybeTruncateText stdoutRes
            <> "\nstderr:\n"
            <> maybeTruncateText stderrRes

gitInit :: FilePath -> IO (Either Text ())
gitInit path = DIR.withCurrentDirectory path $ do
  res <- runProcessWithTimeout 10 "." [] "git" ["init"]
  handleExitCode "'git init'" res

gitSetupUser :: Config -> IO (Either Text ())
gitSetupUser cfg = DIR.withCurrentDirectory (configBaseDir cfg) $ do
  runAll [setupName, setupEmail]
  where
    userName = configGitUserName cfg
    userEmail = configGitUserEmail cfg
    setupName = do
      res <-
        runProcessWithTimeout
          10
          "."
          []
          "git"
          [ "config",
            "user.name",
            T.unpack userName
          ]
      handleExitCode ("'git config user.name " <> userName <> "'") res
    setupEmail = do
      res <-
        runProcessWithTimeout
          10
          "."
          []
          "git"
          [ "config",
            "user.email",
            T.unpack userEmail
          ]
      handleExitCode ("'git config user.email " <> userEmail <> "'") res

gitAdd :: FilePath -> Text -> IO (Either Text ())
gitAdd path name = DIR.withCurrentDirectory path $ do
  res <- runProcessWithTimeout 10 "." [] "git" ["add", T.unpack name]
  handleExitCode ("'git add " <> name <> "'") res

gitCommit :: FilePath -> Text -> IO (Either Text ())
gitCommit path desc = DIR.withCurrentDirectory path $ do
  res <- runProcessWithTimeout 10 "." [] "git" ["commit", "-m", T.unpack desc]
  case res of
    Left err -> return . Left $ "Error running git commit: " <> err
    Right (_, stdoutR, _) ->
      if "nothing added to commit"
        `T.isInfixOf` stdoutR
        || "On branch"
        `T.isInfixOf` stdoutR
        then return $ Right () -- Nothing to commit case - treat as success
        else handleExitCode ("'git commit -m " <> desc <> "'") res

gitAddAndCommit :: Text -> AppM ()
gitAddAndCommit file = do
  cfg <- ask
  let basePath = configBaseDir cfg
  liftIO (runAll [gitAdd basePath file, gitCommit basePath ("Modified " <> file)]) >>= \case
    Left err -> throwError err
    Right () -> pure ()

gitRevertFile :: FilePath -> FilePath -> IO (Either Text ())
gitRevertFile basePath name = DIR.withCurrentDirectory basePath $ do
  res <- runProcessWithTimeout 10 "." [] "git" ["restore", name]
  handleExitCode ("'git restore " <> T.pack name <> "'") res

-- | Check if a binary is present on PATH using findExecutable
checkBinaryOnPath :: Text -> [(String, String)] -> IO Bool
checkBinaryOnPath binaryTxt envVars = do
  -- Get the current environment
  currentEnv <- Env.getEnvironment

  -- Merge current environment with the new variables
  let env = currentEnv ++ envVars
      binary = T.unpack binaryTxt

  -- Extract PATH from the merged environment
  case L.lookup "PATH" env of
    Nothing ->
      -- If no PATH in environment, just use findExecutable with default PATH
      DIR.findExecutable binary >>= return . isJust
    Just pathValue ->
      -- Use bracket to ensure PATH is properly restored even if an exception occurs
      bracket
        ( do
            -- Setup: save current PATH and set new one
            oldPath <- Env.lookupEnv "PATH"
            Env.setEnv "PATH" pathValue
            return oldPath
        )
        ( \oldPath -> do
            -- Cleanup: restore original PATH
            case oldPath of
              Nothing -> Env.unsetEnv "PATH"
              Just path -> Env.setEnv "PATH" path
        )
        ( \_ -> do
            -- Action: check for executable with new PATH
            DIR.findExecutable binary >>= return . isJust
        )
