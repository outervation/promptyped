{-# LANGUAGE OverloadedStrings #-}

module FileSystem (replaceInFile, readFileToText, readFileToTextAndOpen, appendToFile, ensureLineNumbers, toFilePath, getFileNames, fileExistsOnDisk, clearFileOnDisk, runProcessWithTimeout, getFileNamesRecursive, handleExitCode, runAll, gitInit, gitAddAndCommit, ensureNoLineNumbers, addLineNumbersToText, updateOpenedFile, reloadOpenFiles, gitSetupUser, gitRevertFile, tryFileOp) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, bracket, try)
import Control.Monad (foldM)
import Control.Monad.Catch qualified as Catch
import Control.Monad.Except (throwError)
import Core
import Data.ByteString qualified as BS
import Data.Char (isDigit)
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

runAll :: [IO (Either Text ())] -> IO (Either Text ())
runAll = foldM step (Right ())
  where
    step (Left e) _ = pure (Left e) -- short-circuit
    step (Right ()) action = action

toFilePath :: Config -> Text -> FilePath
toFilePath cfg x = configBaseDir cfg FP.</> T.unpack x

tryFileOp :: FilePath -> (FilePath -> IO (Either T.Text ())) -> AppM (Maybe T.Text) -> Maybe FileChangeBounds -> AppM (Either T.Text ())
tryFileOp path op checker maybeBounds = do
  -- Check if file exists first to fail early
  alreadyExists <- liftIO $ DIR.doesFileExist path
  if not alreadyExists
    then liftIO $ op path
    else do
      let backupPath = path ++ ".bak"
          cleanupBackup = liftIO $ tryIOError $ void (DIR.removeFile backupPath)

      -- Use bracket to ensure proper resource management
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

    -- Restore from backup when validation fails
    restoreBackup :: FilePath -> T.Text -> AppM (Either T.Text ())
    restoreBackup backupPath err = do
      modifiedFile <- liftIO $ readFileToText path
      let modifiedFileRelevantPart = getRelevantFilePart $ addLineNumbersToText modifiedFile
      restoreResult <- liftIO $ tryIOError $ do
        DIR.removeFile path
        DIR.copyFile backupPath path

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
          return . Left $ err <> "\n. The relevant part of the file post-modification looked like (note that's not the state of the file on disk, as the change was rejected): " <> modifiedFileRelevantPart

readFileToText :: FilePath -> IO Text
readFileToText path = do
  mayPath <- try @IOException (readFileBS path)
  return $ case mayPath of
    Left _ -> ""
    Right val -> TE.decodeUtf8Lenient val

updateOpenedFile :: Text -> AppM ()
updateOpenedFile fileName = do
  theState <- get
  cfg <- ask
  let fmtErr err = "Internal error: failed ensuring no line numbers for " <> fileName <> ": " <> err :: Text
  case fileAlreadyOpen fileName theState of
    False -> throwError $ "Internal error: tried to update non-opened file " <> fileName
    True ->
      when (isNothing $ isFileForbidden cfg fileName)
        $ liftIO (ensureNoLineNumbers (toFilePath cfg fileName))
        >>= \case
          Left err -> throwError (fmtErr err)
          Right contents -> modify' (updateOpenFile fileName contents)

reloadOpenFiles :: AppM ()
reloadOpenFiles = do
  st <- get
  let openFiles = map openFileName $ stateOpenFiles st
  forM_ openFiles updateOpenedFile

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

removeLineNumberComment :: Text -> Text
removeLineNumberComment line =
  -- \|
  --      Removes an existing `/* digits */` comment at the start of a line
  --      (possibly after some indentation), plus *one space* that follows it,
  --      if present.
  --
  --      For example, if the line is:
  --
  --          "    /* 12 */   let x = 42"
  --
  --      we want to keep the leading indentation `"    "` intact,
  --      remove the whole `"/* 12 */"` (including one space), and
  --      end up with:
  --
  --          "    let x = 42"
  --
  --      Then `addLineNumbers` will prepend a fresh `/* lineNum */` comment
  --      again, without accumulating spaces over multiple runs.
  --

  -- 1. Separate out the leading indentation (or leading spaces).
  let (leadingSpaces, afterIndent) = T.span (== ' ') line
   in case T.stripPrefix "/*" afterIndent of
        Nothing -> line -- does not start with "/*" after indentation
        Just afterOpen ->
          -- afterOpen should look like: " digits */ ...rest..."
          let (digitsPart, afterDigits) = T.breakOn "*/" afterOpen
           in case T.stripPrefix "*/" afterDigits of
                Nothing ->
                  -- There's no "*/" after the "/*" => not a proper comment, leave as-is
                  line
                Just afterClose ->
                  -- Check if the part between "/*" and "*/" is all digits (when stripped).
                  if T.all isDigit (T.strip digitsPart)
                    then
                      -- Remove the comment plus exactly *one* space after it, if that space exists.
                      let afterOneSpace =
                            case T.uncons afterClose of
                              Just (' ', rest) -> rest -- remove one space
                              _ -> afterClose
                       in leadingSpaces <> afterOneSpace
                    else
                      -- The part between "/*" and "*/" wasn't pure digits => keep original line
                      line

addLineNumbers :: V.Vector Text -> V.Vector Text
addLineNumbers = V.imap addComment

addCommentNew :: Int -> Text -> Text
addCommentNew idx originalLine =
  let comment = "/* Line " <> T.pack (show idx) <> " start */"
      endComment = "/* Line " <> T.pack (show idx) <> " end */"
   in if T.null originalLine
        then comment
        else comment <> " " <> originalLine <> " " <> endComment

addComment :: Int -> Text -> Text
addComment idx originalLine =
  let comment = "/* " <> T.pack (show idx) <> " */"
   in if T.null originalLine
        then comment
        else comment <> " " <> originalLine

ensureNoLineNumbers :: FilePath -> IO (Either Text Text)
ensureNoLineNumbers filepath = do
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

addLineNumbersToText :: Text -> Text
addLineNumbersToText contents = do
  let originalLines = V.fromList (T.lines contents)
      processedLines = V.map removeLineNumberComment originalLines
      numberedLines = addLineNumbers processedLines
      newContents = T.unlines (V.toList numberedLines)
  newContents

ensureLineNumbers :: FilePath -> IO (Either Text Text)
ensureLineNumbers filepath = do
  result <- try $ processFileContents filepath :: IO (Either IOException Text)
  pure $ either (Left . T.pack . displayException) Right result
  where
    processFileContents :: FilePath -> IO Text
    processFileContents fp = do
      contents <- readFileToText fp
      let newContents = addLineNumbersToText contents
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
  let allowedDirs = filter ((`notElem` foldersToIgnore) . toText . FP.takeFileName) dirs

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
handleExitCode opName res = do
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
            <> truncateText 30 stdoutRes
            <> "\nstderr:\n"
            <> truncateText 30 stderrRes

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
