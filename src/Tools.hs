{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tools where

import BuildSystem qualified as BS
import Control.Monad.Except
import Text.Regex.PCRE qualified as PCRE
import Core
import Data.Aeson as AE
import Data.Aeson.Types qualified as AET
import Data.ByteString.Lazy qualified as LBS
import Data.List as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import FileSystem qualified as FS
import Relude
import ShapeChecker (checkShapesMatch)

data Tool = ToolOpenFile | ToolCloseFile | ToolAppendFile | ToolReplaceFile | ToolInsertInFile | ToolEditFile | ToolRevertFile | ToolPanic | ToolReturn
  deriving (Eq, Ord, Show)

data ToolCall a = ToolCallOpenFile [OpenFileArg] | ToolCallCloseFile [CloseFileArg] | ToolCallAppendFile [AppendFileArg] | ToolCallReplaceFile [AppendFileArg] | ToolCallEditFile [EditFileArg] | ToolCallRevertFile [RevertFileArg] | ToolCallInsertInFile [InsertInFileArg] | ToolCallPanic PanicArg | ToolCallReturn a
  deriving (Generic, Eq, Ord, Show)

instance (ToJSON a) => ToJSON (ToolCall a)

instance (FromJSON a) => FromJSON (ToolCall a)

toolCallTool :: ToolCall a -> Tool
toolCallTool (ToolCallOpenFile _) = ToolOpenFile
toolCallTool (ToolCallCloseFile _) = ToolCloseFile
toolCallTool (ToolCallAppendFile _) = ToolAppendFile
toolCallTool (ToolCallReplaceFile _) = ToolReplaceFile
toolCallTool (ToolCallEditFile _) = ToolEditFile
toolCallTool (ToolCallInsertInFile _) = ToolInsertInFile
toolCallTool (ToolCallRevertFile _) = ToolRevertFile
toolCallTool (ToolCallPanic _) = ToolPanic
toolCallTool (ToolCallReturn _) = ToolReturn

getReturn :: [ToolCall a] -> Maybe a
getReturn [] = Nothing
getReturn (ToolCallReturn x : _) = Just x
getReturn (_ : rest) = getReturn rest

data OpenFileArg = OpenFileArg
  { fileName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON OpenFileArg

instance FromJSON OpenFileArg

data CloseFileArg = CloseFileArg
  { fileName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON CloseFileArg

instance FromJSON CloseFileArg

data AppendFileArg = AppendFileArg
  { fileName :: Text,
    text :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON AppendFileArg

instance FromJSON AppendFileArg

data EditFileArg = EditFileArg
  { fileName :: Text,
    startLineNum :: Int,
    endLineNum :: Int,
    text :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON EditFileArg

instance FromJSON EditFileArg

data InsertInFileArg = InsertInFileArg
  { fileName :: Text,
    lineNum :: Int,
    text :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON InsertInFileArg

instance FromJSON InsertInFileArg

data RevertFileArg = RevertFileArg
  { fileName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON RevertFileArg

instance FromJSON RevertFileArg

truncateAppendFileArg :: AppendFileArg -> AppendFileArg
truncateAppendFileArg (AppendFileArg name text) = AppendFileArg name $ T.take 100 text

truncateEditFileArg :: EditFileArg -> EditFileArg
truncateEditFileArg (EditFileArg name start end text) = EditFileArg name start end $ T.take 100 text

truncateInsertInFileArg :: InsertInFileArg -> InsertInFileArg
truncateInsertInFileArg (InsertInFileArg name lineNum text) = InsertInFileArg name lineNum $ T.take 100 text

-- Note: reverse sort so later line numbers are modified first,
-- to avoid the meaning of line numbers changing
validateAndSortEditFileArgs :: [EditFileArg] -> Either Text [EditFileArg]
validateAndSortEditFileArgs args = do
  -- First validate all line numbers
  validateEditFileLineNumbers args
  -- Then check for overlaps within each file
  validateEditFileNoOverlaps args
  -- If all validations pass, return sorted list
  Right $ sortBy compareEditFileArgs args

-- Compare function for sorting
compareEditFileArgs :: EditFileArg -> EditFileArg -> Ordering
compareEditFileArgs a b = case compare a.fileName b.fileName of
  EQ -> compare (startLineNum b) (startLineNum a)
  other -> other

validateAndSortInsertInFileArgs :: [InsertInFileArg] -> Either Text [InsertInFileArg]
validateAndSortInsertInFileArgs args = Right $ sortBy compareInsertInFileArgs args

-- Compare function for sorting
compareInsertInFileArgs :: InsertInFileArg -> InsertInFileArg -> Ordering
compareInsertInFileArgs a b = case compare a.fileName b.fileName of
  EQ -> compare (lineNum b) (lineNum a)
  other -> other

-- Validate individual line numbers
validateEditFileLineNumbers :: [EditFileArg] -> Either Text ()
validateEditFileLineNumbers args =
  case filter invalidLineNumbers args of
    [] -> Right ()
    (arg : _) ->
      Left
        $ T.concat
          [ "Invalid line numbers in for EditFile: ",
            arg.fileName,
            " start: ",
            T.pack (show $ startLineNum arg),
            " end: ",
            T.pack (show $ endLineNum arg)
          ]
  where
    invalidLineNumbers arg =
      startLineNum arg
        < 0
        || endLineNum arg
        < 0
        || endLineNum arg
        < startLineNum arg

-- Validate no overlapping ranges within same file
validateEditFileNoOverlaps :: [EditFileArg] -> Either Text ()
validateEditFileNoOverlaps args =
  let groupedByFile =
        L.groupBy (\a b -> a.fileName == b.fileName)
          $ sortBy compareEditFileArgs args
   in case findFirstEditFileOverlap groupedByFile of
        Nothing -> Right ()
        Just (a, b) ->
          Left
            $ T.concat
              [ "Overlapping ranges in file: ",
                a.fileName,
                " between ",
                T.pack (show (startLineNum a, endLineNum a)),
                " and ",
                T.pack (show (startLineNum b, endLineNum b))
              ]

-- Helper to find first overlap in sorted groups
findFirstEditFileOverlap :: [[EditFileArg]] -> Maybe (EditFileArg, EditFileArg)
findFirstEditFileOverlap = getFirst . map findOverlapInGroup
  where
    getFirst [] = Nothing
    getFirst (Nothing : rest) = getFirst rest
    getFirst (Just x : _) = Just x

    findOverlapInGroup [] = Nothing
    findOverlapInGroup [_] = Nothing
    findOverlapInGroup (a : b : rest) =
      if rangesOverlap a b
        then Just (a, b)
        else findOverlapInGroup (b : rest)

    rangesOverlap a b =
      startLineNum a
        < endLineNum b
        && endLineNum a
        >= startLineNum b

data PanicArg = PanicArg
  { reason :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON PanicArg

instance FromJSON PanicArg

toolName :: Tool -> Text
toolName x = T.drop 4 $ show x

toolSummary :: Text
toolSummary = "You have the following tools available to you, that you may call with JSON args. Line numbers are included for your OpenFiles to simplify your task, but are not present in the files on disk (so don't explicitly write line numbers to disk!). For tools that modify files, after modification the file will be compiled if a source file, and run if it's a unit test file. NOTE: the JSON must be standard; long strings must be expressed as a single string, not multi-line strings in Python \"line1\" \n \"line2\" style, as the parser doesn't support concatenating such strings. Also, please DO NOT include comments in the JSON."

toJObj :: AET.Object -> Text
toJObj = TE.decodeUtf8Lenient . LBS.toStrict . AE.encode

toJ :: (ToJSON a) => a -> Text
toJ = TE.decodeUtf8Lenient . LBS.toStrict . encode

textToObj :: T.Text -> Either String Object
textToObj = eitherDecode . LBS.fromStrict . TE.encodeUtf8

fromJObj :: (FromJSON a) => AET.Object -> AppM a
fromJObj obj = do
  let res = fromJSON (Object obj)
  liftEither $ case res of
    Success val -> Right val
    Error err -> Left $ "Internal error: " <> T.pack err <> ", incorrect arg format json: " <> show obj

fromJ :: (FromJSON a) => Text -> AppM a
fromJ txt = do
  case textToObj txt of
    Left err -> throwError (T.pack err)
    Right obj -> fromJObj obj

toolArgFormatAndDesc :: Tool -> (Text, Text)
toolArgFormatAndDesc ToolReturn = ("{ }", "Return a value; format depends on the task and is described further down below.")
toolArgFormatAndDesc ToolOpenFile = (toJ OpenFileArg {fileName = "someFile.go"}, "Load a file into the context")
toolArgFormatAndDesc ToolCloseFile = (toJ CloseFileArg {fileName = "someFile.go"}, "Remove a file from the context")
toolArgFormatAndDesc ToolAppendFile = (toJ AppendFileArg {fileName = "somefile.go", text = "someCodeHere()"}, "Append code/text to a file. Can be used to create a new file.")
toolArgFormatAndDesc ToolReplaceFile = (toJ AppendFileArg {fileName = "somefile.go", text = "someCodeHere()"}, "Replace a file with the provided code/text to a file. Can be used to create a new file. Prefer this over editing when the file is small.")
toolArgFormatAndDesc ToolEditFile = (toJ EditFileArg {fileName = "somefile.go", startLineNum = 5, endLineNum = 10, text = "someCodeHere()"}, "Replace text in [startLineNum, endLineNum] with the text you provide. Note if making multiple edits to the same file, the start/end line numbers of different edits cannot overlap. IMPORTANT: if you insert more lines than you're replacing, the rest will be inserted, not replaced. So inserting 2 lines at at startLineNum=15 endLineNum=15 will only replace the existing line 15 in the file, and add the second provided line after that, it won't replace lines 15 and 16. Note too that the line-numbers are provided to you at the START of the line in every file.")
toolArgFormatAndDesc ToolInsertInFile = (toJ InsertInFileArg {fileName = "somefile.go", lineNum = 17, text = "someCodeHere()"}, "Insert the provided text into the file at lineNum, not replacing/overwriting the content on that line (instead it's moved to below the inserted text).")
toolArgFormatAndDesc ToolRevertFile = (toJ RevertFileArg {fileName = "someFile.go"}, "Revert un-added changes in an open file; changes are committed when compilation and unit tests succeed, so will revert to the last version of the file before compilation or unit tests failed. Use this if you get the file in a state you can't recover it from.")
toolArgFormatAndDesc ToolPanic = (toJ PanicArg {reason = "This task is impossible for me to do because ..."}, "Call this if you can't complete the task due to it being impossible or not having enough information")

mkToolCallSyntax :: Tool -> Text -> Text
mkToolCallSyntax tool argFormat = toolName tool <> "=<[" <> argFormat <> "]>"

toolToDescription :: Tool -> Text
toolToDescription x = do
  let (argFormat, toolDesc) = toolArgFormatAndDesc x
  "Syntax: " <> mkToolCallSyntax x argFormat <> " Description: " <> toolDesc

returnValueToDescription :: (ToJSON a) => a -> Text
returnValueToDescription example = do
  let exampleTxt = toJ example
  let fmt = "You must return the output in a format like the following: " <> mkToolCallSyntax ToolReturn exampleTxt
  fmt <> " \n You must either return a value or call a tool. Because you're part of an automated process, you cannot prompt the user for information, so panic if you don't know how to proceed."

toolsToDescription :: [Tool] -> Text
toolsToDescription tools = toolSummary <> "\nAll available tools: \n" <> T.unlines (map toolToDescription tools) <> "\n Multiple tool calls are supported, you can either do ToolName<[{jsonArgs}]>, ToolName<[{otherJsonArgs}]>, or ToolName<[{jsonArgs}, {otherJsonArgs}]>; both are supported. Note for file names, nested paths (e.g. somedir/somefile.txt) are NOT supported, only direct paths like somefile.txt are."

tmp :: Text
tmp = "AppendFile<[{\"fileName\":\"websocket_client.h\",\"text\":\"#pragma once\\n\\n#include <libwebsockets.h>\\n#include \\\"config.h\\\"\\n#include \\\"simdjson.h\\\"\\n#include \\\"book_data.h\\\"\\n#include \\\"trade_data.h\\\"\\n#include <spdlog/spdlog.h>\\n#include <functional>\\n\\nnamespace websocket {\\n\\nstruct Handler {\\n    virtual void on_trade(const trade_data::TradeEvent& trade) = 0;\\n    virtual void on_agg_trade(const trade_data::AggTradeEvent& agg_trade) = 0;\\n    virtual void on_book_update(const book_data::BookUpdate& update) = 0;\\n    virtual void on_best_bid_ask(const book_data::BestBidAsk& update) = 0;\\n    virtual void request_snapshot(const std::string& symbol) = 0;\\n    virtual ~Handler() = default;\\n};\\n\\nnamespace core {\\n    bool check_sequence_gap(uint64_t last_update_id, const book_data::BookUpdate& update);\\n    void process_message(simdjson::ondemand::document& doc, Handler& handler);\\n}\\n\\nclass WebSocketClient {\\npublic:\\n    WebSocketClient(config::BinanceConfig config, Handler& handler);\\n    ~WebSocketClient();\\n\\n    void connect();\\n    void poll(int timeout_ms = 0);\\n\\nprivate:\\n    static int lws_callback(lws* wsi, lws_callback_reasons reason, void* user, void* in, size_t len);\\n    int handle_callback(lws* wsi, lws_callback_reasons reason, void* in, size_t len);\\n\\n    lws_context* context = nullptr;\\n    lws* wsi = nullptr;\\n    config::BinanceConfig config;\\n    Handler& handler;\\n    simdjson::ondemand::parser json_parser;\\n};\\n\\n} // namespace websocket\\n\"}]>\nOpenFile=<[{\"fileName\":\"websocket_client.h\"}]>\n\nReturn=<[{\"createdFiles\":[{\"createdFileName\":\"websocket_client.h\",\"createdFileSummary\":\"Libwebsockets wrapper for Binance with message processing core. Handles WS connection lifecycle, message parsing using simdjson, sequence gap detection, and event dispatch to handler interfaces. Separates pure message validation (check_sequence_gap) from IO-bound WS ops. Uses config::BinanceConfig for endpoints and symbols. Pure core logic in namespace allows testing without live connection.\"}]}]>"

-- | Extract all occurrences of the pattern:
--
--    RAWSTRING[someName]=[R| ...contents... |R]
--
-- Returns a list of (someName, contents) pairs.
extractRawStrings :: Text -> [(Text, Text)]
extractRawStrings input =
  -- The '(?s)' inline modifier makes '.' match newlines as well, allowing
  -- multiline contents in the second capturing group.
  let pattern  = "RAWSTRING\\[([^\\]]+)\\]=\\[R\\|(?s)(.*?)\\|R\\]" :: String
      -- The ':: [[Text]]' means we get a list of matches,
      -- each match is a list of captured groups:
      --   index 0 = the entire match
      --   index 1 = the text matching ([^\\]]+)
      --   index 2 = the text matching (?s)(.*?)
      allMatches :: [[String]]
      allMatches = (T.unpack input) PCRE.=~ pattern
   in map (\matchGroups -> (T.pack (matchGroups !! 1), T.pack (matchGroups !! 2))) allMatches

extractFnCalls :: Text -> Text -> Either Text [AET.Object]
extractFnCalls fullText fnName =
  let ------------------------------------------------------------------------
      -- 1. Gather matches for both patterns: "fnName=<" and "fnName<"
      ------------------------------------------------------------------------
      patternEq = fnName <> "=<" -- e.g. "OpenFile=<"
      patternNoEq = fnName <> "<" -- e.g. "OpenFile<"

      -- T.breakOnAll returns [(before, matchAndAfter)] for every occurrence
      matchesEq =
        [ (before, after, T.length patternEq)
        | (before, after) <- T.breakOnAll patternEq fullText
        ]

      matchesNoEq =
        [ (before, after, T.length patternNoEq)
        | (before, after) <- T.breakOnAll patternNoEq fullText
        ]

      -- Combine matches and sort them in ascending order of where they occur.
      allMatches = matchesEq ++ matchesNoEq
      sortedMatches = sortOn (\(before, _after, _plen) -> T.length before) allMatches

      ------------------------------------------------------------------------
      -- 2. Parse each match by skipping the matched pattern and reading "[...]>",
      --    exactly as in your original code.
      ------------------------------------------------------------------------
      parseMatch :: (Text, Text, Int) -> Either Text [AET.Object]
      parseMatch (_before, afterPrefix, patternLen) =
        let -- Skip the pattern (either fnName=< or fnName<):
            content = T.drop patternLen afterPrefix

            -- We expect a JSON array starting with '[' up to the next "]>".
            (jsonCandidate, _rest) = T.breakOn "]>" content
            jsonCandidate' = jsonCandidate <> "]"
            bs = TE.encodeUtf8 jsonCandidate'
         in case AE.eitherDecodeStrict' bs of
              Left err ->
                Left
                  $ "Invalid JSON array for "
                  <> fnName
                  <> " due to "
                  <> T.pack err
                  <> ": "
                  <> jsonCandidate
                  <> ". NOTE: the JSON must be standard; "
                  <> "no multiline or concatenated strings, no comments, etc."
              Right (AE.Array arrVal) ->
                -- Ensure every element in the array is a JSON object
                let vals = V.toList arrVal
                 in case traverse ensureObject vals of
                      Left msg -> Left msg
                      Right objs -> Right objs
              Right _ ->
                Left $ "Expected a JSON array but got something else: " <> jsonCandidate

      ensureObject :: AE.Value -> Either Text AET.Object
      ensureObject val =
        case val of
          AE.Object o -> Right o
          _ -> Left ("Array element is not an object: " <> T.pack (show val))

      -- Process all matches, collecting errors or successes
      results = map parseMatch sortedMatches
      (errs, successes) = partitionEithers results
   in ------------------------------------------------------------------------
      -- 3. Return either a combined error or all parsed objects
      ------------------------------------------------------------------------
      if null allMatches
        then Right [] -- no calls found -> empty list is OK
        else
          if not (null errs)
            then Left (T.intercalate "\n\n" errs)
            else Right (concat successes)

--------------------------------------------------------------------------------
-- 2. Find calls for *all* tools in the text, returning either:
--       Left <error text>
--    or
--       Right [(Tool, [Aeson.Object])]
--
--    If any tool that *does appear* in the text fails to parse, we produce a
--    Left with the concatenated errors. If a tool does not appear at all, it
--    just yields an empty list for that tool, which we omit in the final result.
--------------------------------------------------------------------------------

findToolsCalled :: Text -> [Tool] -> Either Text [(Tool, [AET.Object])]
findToolsCalled txt tools =
  let -- For each tool, try extracting all JSON objects
      attempts :: [(Tool, Either Text [AET.Object])]
      attempts =
        [ (tool, extractFnCalls txt (toolName tool))
        | tool <- tools
        ]

      -- We only keep entries for which we either got a parse error or at least
      -- one object. If we got `Right []`, that means the tool never appeared
      -- in the text at all, so we can safely ignore it in the final result.
      relevant =
        [ (tool, eObjs)
        | (tool, eObjs) <- attempts,
          case eObjs of
            Right [] -> False -- no occurrences => ignore
            _ -> True -- either an error, or we got some objects
        ]

      -- Now partition them into parse errors and successes.
      (parseErrors, parsedOK) =
        partitionEithers
          [ case eObjs of
              Left err -> Left err
              Right objs -> Right (tool, objs)
          | (tool, eObjs) <- relevant
          ]
   in case parsedOK of
        [] ->
          -- Means either no tools matched or all had parse errors.  If we
          -- want to distinguish "no matches at all" from "all parse
          -- errors," we can do so here.  For simplicity, say:
          if null parseErrors
            then Left "No tool calls found. Remember the syntax is ToolName=<[{ someJson }]> , not ToolName=[{ someJson }] and not ToolName<[{ someJson }]> (replace ToolName here with the actual name of the tool; ToolName itself is not a tool!)"
            else Left (T.intercalate ", " parseErrors)
        _ ->
          -- We do have at least one success. If any had errors, produce a
          -- combined error. Otherwise return all successes.
          if null parseErrors
            then Right parsedOK
            else Left (T.intercalate ", " parseErrors)

checkToolArgs :: Tool -> AET.Object -> Either Text ()
checkToolArgs ToolReturn _ = Right ()
checkToolArgs tool obj = do
  let argFmt = fst $ toolArgFormatAndDesc tool
  let sampleObj = AE.decode (encodeUtf8 argFmt)
  case sampleObj of
    Just sample -> first (("Shape of args to " <> toolName tool <> " failed to match reference " <> argFmt <> ": ") <>) $ checkShapesMatch (Object sample) (Object obj)
    Nothing -> Left $ "Internal error: invalid sample json " <> argFmt

processArgsOfType :: (FromJSON a) => Tool -> [AET.Object] -> Either Text [a]
processArgsOfType tool args = do
  -- First validate all args with checkToolArgs
  let validationResults = map (checkToolArgs tool) args
  case partitionEithers validationResults of
    (errors@(_ : _), _) -> Left $ T.intercalate ", " errors
    ([], _) -> do
      -- Then try parsing each object
      let parseResults = map (AET.fromJSON . AET.Object) args
      case partitionParsing parseResults of
        (errors@(_ : _), _) -> Left $ T.intercalate ", " errors
        ([], values) -> Right values
  where
    partitionParsing :: [Result a] -> ([Text], [a])
    partitionParsing = foldr accumResult ([], [])
      where
        accumResult (Error err) (errs, vals) = (T.pack err : errs, vals)
        accumResult (Success val) (errs, vals) = (errs, val : vals)

processArgOfType :: (FromJSON a, Show a) => Tool -> [AET.Object] -> Either Text a
processArgOfType tool args = case processArgsOfType tool args of
  Left err -> Left err
  Right vals -> case vals of
    [x] -> Right x
    oth -> Left $ "Expected single arg for " <> toolName tool <> " but got " <> show oth

processToolArgs :: (FromJSON a, Show a) => Tool -> [AET.Object] -> Either Text (ToolCall a)
processToolArgs tool@ToolOpenFile args = ToolCallOpenFile <$> processArgsOfType tool args
processToolArgs tool@ToolCloseFile args = ToolCallCloseFile <$> processArgsOfType tool args
processToolArgs tool@ToolAppendFile args = ToolCallAppendFile <$> processArgsOfType tool args
processToolArgs tool@ToolReplaceFile args = ToolCallReplaceFile <$> processArgsOfType tool args
processToolArgs tool@ToolEditFile args = ToolCallEditFile <$> processArgsOfType tool args
processToolArgs tool@ToolInsertInFile args = ToolCallInsertInFile <$> processArgsOfType tool args
processToolArgs tool@ToolRevertFile args = ToolCallRevertFile <$> processArgsOfType tool args
processToolArgs tool@ToolPanic args = ToolCallPanic <$> processArgOfType tool args
processToolArgs tool@ToolReturn args = ToolCallReturn <$> processArgOfType tool args

processToolsArgs :: (FromJSON a, Show a) => [(Tool, [AET.Object])] -> Either Text [ToolCall a]
processToolsArgs toolArgs = case partitionEithers (map (uncurry processToolArgs) toolArgs) of
  ([], results) -> Right results
  (errors, _) -> Left (T.intercalate ", " errors)

mkSuccess :: Context -> MsgKind -> Text -> Context
mkSuccess ctxt kind = addToContextUser ctxt kind . mappend "Success: "

mkError :: Context -> MsgKind -> Text -> Context
mkError ctxt kind = addToContextUser ctxt kind . mappend "Error: "

reloadLogs :: AppM ()
reloadLogs = do
  st <- get
  updatedFiles <- traverse updateFile (stateOpenFiles st)
  put $ st {stateOpenFiles = updatedFiles}
  where
    updateFile :: OpenFile -> AppM OpenFile
    updateFile f@(OpenFile fileName _) =
      if ".log" `T.isSuffixOf` fileName
        then do
          cfg <- ask
          newContents <- liftIO $ FS.readFileToText (FS.toFilePath cfg fileName)
          return $ f {openFileContents = newContents}
        else return f

considerBuildAndTest :: forall a. (BS.BuildSystem a) => Text -> AppM (Maybe (MsgKind, Text))
considerBuildAndTest fileName = do
  isBuildable <- BS.isBuildableFile @a fileName
  cfg <- ask
  liftIO $ putTextLn $ "Considering " <> fileName <> "; isBuildable=" <> show isBuildable
  let baseDir = configBaseDir cfg
  case isBuildable of
    False -> return Nothing
    True -> do
      timeIONano64M (BS.buildProject @a cfg) >>= \case
        (Just err, compileNanos) -> do
          liftIO $ putTextLn $ "Compilation failed: " <> err
          modify' $ updateLastCompileState (Just err)
          modify' $ updateStateMetrics (mempty {metricsNumCompileFails = 1, metricsCompileTime = compileNanos})
          FS.reloadOpenFiles
          return $ Just (CompileFailMsg, err)
        (Nothing, compileNanos) -> do
          modify' $ updateLastCompileState Nothing
          (result, testNanos) <- timeIONano64M $ BS.testProject @a cfg
          existingFileNames <- liftIO $ FS.getFileNamesRecursive ["build", "contrib", ".git"] baseDir
          modify' (updateExistingFiles existingFileNames)
          reloadLogs
          FS.reloadOpenFiles
          modify' $ updateLastTestState result
          when (isJust result) $ modify $ updateStateMetrics (mempty {metricsNumTestFails = 1, metricsCompileTime = compileNanos, metricsTestTime = testNanos})
          return $ fmap (TestFailMsg,) result

data RequiresOpenFile = RequiresOpenFileTrue | RequiresOpenFileFalse
  deriving (Eq, Ord, Show)

handleFileOperation ::
  forall a.
  (BS.BuildSystem a) =>
  Text ->
  (FilePath -> IO (Either Text ())) ->
  RequiresOpenFile ->
  Text ->
  Text ->
  Context ->
  AppM Context
handleFileOperation fileName ioAction requiresOpenFile errorPrefix successMsg ctxt = do
  theState <- get
  cfg <- ask
  case isFileForbidden cfg fileName of
    Just err -> pure $ mkError ctxt OtherMsg $ "Error: cannot modify forbidden file. " <> err
    Nothing -> do
      let alreadyOpen = fileAlreadyOpen fileName theState
      if alreadyOpen || requiresOpenFile == RequiresOpenFileFalse
        then do
          res <- liftIO $ ioAction (FS.toFilePath cfg fileName)
          either (pure . mkError ctxt OtherMsg) (const $ onSuccess cfg ctxt) res
        else pure $ mkError ctxt OtherMsg (errorPrefix <> fileName)
  where
    onSuccess cfg ctxt' = do
      theState <- get
      unless (fileExists fileName theState) $ modify' (addExistingFile fileName "")
      openFile fileName cfg
      let successCtxt = mkSuccess ctxt' OtherMsg successMsg
      considerBuildAndTest @a fileName >>= \case
        Nothing -> do
          FS.gitAddAndCommit fileName
          pure successCtxt
        Just (msgKind, err) -> pure $ mkError successCtxt msgKind (show msgKind <> ": " <> err)

openFile :: Text -> Config -> AppM ()
openFile fileName cfg = do
  contents <- liftIO $ FS.readFileToTextAndOpen (FS.toFilePath cfg fileName)
  modify' (ensureOpenFile fileName contents)
  FS.updateOpenedFile fileName

runTool :: forall bs a. (ToJSON a, FromJSON a, Show a, BS.BuildSystem bs) => ToolCall a -> Context -> AppM Context
runTool (ToolCallOpenFile args) origCtxt = do
  theState <- get
  cfg <- ask
  let initialCtxt = origCtxt
  ctxtUpdates <- forM args $ \(OpenFileArg fileName) -> do
    let exists = fileExists fileName theState
        alreadyOpen = fileAlreadyOpen fileName theState
    case alreadyOpen of
      True -> pure $ \ctxt -> mkError ctxt OtherMsg ("file already open: " <> fileName)
      False -> do
        openFile fileName cfg
        unless exists $ modify' (addExistingFile fileName "")
        pure $ \ctxt -> mkSuccess ctxt OtherMsg ("Opened file: " <> fileName)
  return $ foldl' (\acc f -> f acc) initialCtxt ctxtUpdates
runTool (ToolCallCloseFile args) origCtxt = do
  theState <- get
  let initialCtxt = origCtxt
  ctxtUpdates <- forM args $ \(CloseFileArg fileName) -> do
    let alreadyOpen = fileAlreadyOpen fileName theState
    case alreadyOpen of
      False -> pure $ \ctxt -> mkError ctxt OtherMsg ("cannot close file that isn't open: " <> fileName)
      True -> do
        modify' (closeOpenFile fileName)
        pure $ \ctxt -> mkSuccess ctxt OtherMsg ("Closed file: " <> fileName)
  return $ foldl' (\acc f -> f acc) initialCtxt ctxtUpdates
runTool (ToolCallAppendFile args) origCtxt = do
  let initialCtxt = origCtxt
  ctxtUpdates <- forM args $ \(AppendFileArg fileName txt) -> pure $ \ctxt ->
    handleFileOperation @bs
      fileName
      (`FS.appendToFile` txt)
      RequiresOpenFileFalse
      "cannot append to file that hasn't been opened: "
      ("Appended to file: " <> fileName)
      ctxt
  foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool (ToolCallReplaceFile args) origCtxt = do
  let initialCtxt = origCtxt
  let replaceFile txt fileName = FS.clearFileOnDisk fileName >> FS.appendToFile fileName txt
  ctxtUpdates <- forM args $ \(AppendFileArg fileName txt) -> pure $ \ctxt ->
    handleFileOperation @bs
      fileName
      (replaceFile txt)
      RequiresOpenFileFalse
      "cannot replace file that hasn't been opened: "
      ("Replaced file: " <> fileName)
      ctxt
  foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool (ToolCallInsertInFile args) origCtxt = do
  let initialCtxt = origCtxt
      sortedArgAttempt = validateAndSortInsertInFileArgs args
  case sortedArgAttempt of
    Left err -> pure $ mkError initialCtxt OtherMsg ("Error in InsertInFile arguments: " <> err)
    Right sortedArgs -> do
      ctxtUpdates <- forM sortedArgs $ \(InsertInFileArg fileName lineNum txt) -> pure $ \ctxt ->
        handleFileOperation @bs
          fileName
          (\path -> FS.replaceInFile path lineNum lineNum txt)
          RequiresOpenFileTrue
          "cannot insert in file that hasn't been opened: "
          ("Inserted into file: " <> fileName <> " at line " <> show lineNum)
          ctxt
      foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool (ToolCallEditFile args) origCtxt = do
  let initialCtxt = origCtxt
      sortedArgAttempt = validateAndSortEditFileArgs args
  case sortedArgAttempt of
    Left err -> pure $ mkError initialCtxt OtherMsg ("Error in EditFile arguments: " <> err)
    Right sortedArgs -> do
      ctxtUpdates <- forM sortedArgs $ \(EditFileArg fileName startLineNum endLineNum txt) -> pure $ \ctxt ->
        handleFileOperation @bs
          fileName
          (\path -> FS.replaceInFile path startLineNum (endLineNum + 1) txt)
          RequiresOpenFileTrue
          "cannot edit file that hasn't been opened: "
          ("Edited file: " <> fileName)
          ctxt
      foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool (ToolCallRevertFile args) origCtxt = do
  cfg <- ask
  let initialCtxt = origCtxt
      baseDir = configBaseDir cfg
  ctxtUpdates <- forM args $ \(RevertFileArg fileName) -> pure $ \ctxt ->
    handleFileOperation @bs
      fileName
      (FS.gitRevertFile baseDir)
      RequiresOpenFileTrue
      "cannot revert file that hasn't been opened: "
      ("Reverted file: " <> fileName)
      ctxt
  foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool (ToolCallPanic arg) _ = throwError $ "AI panicked due to reason= " <> show arg
runTool (ToolCallReturn arg) ctxt = pure $ addToContextUser ctxt OtherMsg ("Attempted to return value: " <> show arg)

data MockCreatedFile = MockCreatedFile
  { createdFileName :: Text,
    createdFileSummary :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON MockCreatedFile

instance FromJSON MockCreatedFile

data MockCreatedFiles = MockCreatedFiles
  { createdFiles :: [MockCreatedFile]
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON MockCreatedFiles

instance FromJSON MockCreatedFiles

main :: IO ()
main = do
  let tools = [ToolOpenFile, ToolCloseFile, ToolAppendFile, ToolEditFile, ToolReplaceFile, ToolPanic, ToolReturn]
  case findToolsCalled tmp tools of
    Left err -> putStrLn $ "Error: " <> T.unpack err
    Right results -> do
      putStrLn "Successfully parsed tools:"
      mapM_ print (processToolsArgs @MockCreatedFiles results)
