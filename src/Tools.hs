{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tools where

import BuildSystem qualified as BS
import TerminalInput qualified
import Control.Monad.Except
import Control.Monad (foldM)
import Core
import Data.Aeson as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as M
import Data.Aeson.Types qualified as AET
import Data.ByteString.Lazy qualified as LBS
import Data.List as L
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import FileSystem qualified as FS
import Logging qualified
import Relude
import ShapeChecker (checkShapesMatch)
import Text.Regex (subRegex, mkRegex)
import Text.Regex.Base (makeRegexM)
import Text.Regex.TDFA (Regex, match)

maxLineDistanceForRegexLineMatch :: Int
maxLineDistanceForRegexLineMatch = 20

data Tool = ToolOpenFile | ToolFocusFile | ToolCloseFile | ToolAppendFile | ToolReplaceFile | ToolInsertInFile | ToolEditFile | ToolEditFileByMatch | ToolRevertFile | ToolFileLineOp | ToolAddDependency | ToolPanic | ToolEscalate | ToolSummariseAction | ToolReturn
  deriving (Eq, Ord, Show)

isMutation :: Tool -> Bool
isMutation ToolReturn = False
isMutation ToolPanic = False
isMutation ToolEscalate = False
isMutation ToolSummariseAction = False
isMutation _ = True

isEditor :: Tool -> Bool
isEditor ToolEditFile = True
isEditor ToolEditFileByMatch = True
isEditor ToolFileLineOp = True
isEditor _ = False

data ToolCall a = ToolCallOpenFile [OpenFileArg] | ToolCallFocusFile [FocusFileArg] | ToolCallCloseFile [CloseFileArg] | ToolCallAppendFile [AppendFileArg] | ToolCallReplaceFile [AppendFileArg] | ToolCallEditFile [EditFileArg] | ToolCallEditFileByMatch [EditFileByMatchArg] | ToolCallRevertFile [RevertFileArg] | ToolCallInsertInFile [InsertInFileArg] | ToolCallFileLineOp [FileLineOpArg] | ToolCallAddDependency [AddDependencyArg]| ToolCallPanic PanicArg | ToolCallEscalate EscalateArg | ToolCallSummariseAction [SummariseActionArg] | ToolCallReturn a
  deriving (Generic, Eq, Ord, Show)

instance (ToJSON a) => ToJSON (ToolCall a)

instance (FromJSON a) => FromJSON (ToolCall a)

toolCallTool :: ToolCall a -> Tool
toolCallTool (ToolCallOpenFile _) = ToolOpenFile
toolCallTool (ToolCallFocusFile _) = ToolFocusFile
toolCallTool (ToolCallCloseFile _) = ToolCloseFile
toolCallTool (ToolCallAppendFile _) = ToolAppendFile
toolCallTool (ToolCallReplaceFile _) = ToolReplaceFile
toolCallTool (ToolCallEditFile _) = ToolEditFile
toolCallTool (ToolCallEditFileByMatch _) = ToolEditFileByMatch
toolCallTool (ToolCallInsertInFile _) = ToolInsertInFile
toolCallTool (ToolCallRevertFile _) = ToolRevertFile
toolCallTool (ToolCallPanic _) = ToolPanic
toolCallTool (ToolCallEscalate _) = ToolEscalate
toolCallTool (ToolCallSummariseAction _) = ToolSummariseAction
toolCallTool (ToolCallReturn _) = ToolReturn
toolCallTool (ToolCallFileLineOp _) = ToolFileLineOp
toolCallTool (ToolCallAddDependency _) = ToolAddDependency

getReturn :: [ToolCall a] -> Maybe a
getReturn [] = Nothing
getReturn (ToolCallReturn x : _) = Just x
getReturn (_ : rest) = getReturn rest

mergeToolCalls :: [ToolCall a] -> [ToolCall a]
mergeToolCalls =
  -- group all consecutive ToolCalls that share the same constructor
  concatMap mergeGroup . groupBy ((==) `on` toolCallTool)
  where
    mergeGroup :: [ToolCall a] -> [ToolCall a]
    mergeGroup [] = []
    mergeGroup grp@(g : _) =
      case g of
        ToolCallOpenFile _ ->
          -- Flatten all the [OpenFileArg]s into one list.
          [ ToolCallOpenFile
              (concat [as | ToolCallOpenFile as <- grp])
          ]
        ToolCallFocusFile _ ->
          -- Flatten all the [OpenFileArg]s into one list.
          [ ToolCallFocusFile
              (concat [as | ToolCallFocusFile as <- grp])
          ]
        ToolCallCloseFile _ ->
          [ ToolCallCloseFile
              (concat [as | ToolCallCloseFile as <- grp])
          ]
        ToolCallAppendFile _ ->
          [ ToolCallAppendFile
              (concat [as | ToolCallAppendFile as <- grp])
          ]
        ToolCallReplaceFile _ ->
          [ ToolCallReplaceFile
              (concat [as | ToolCallReplaceFile as <- grp])
          ]
        ToolCallInsertInFile _ ->
          [ ToolCallInsertInFile
              (concat [as | ToolCallInsertInFile as <- grp])
          ]
        ToolCallEditFile _ ->
          [ ToolCallEditFile
              (concat [as | ToolCallEditFile as <- grp])
          ]
        ToolCallEditFileByMatch _ ->
          [ ToolCallEditFileByMatch
              (concat [as | ToolCallEditFileByMatch as <- grp])
          ]
        ToolCallRevertFile _ ->
          [ ToolCallRevertFile
              (concat [as | ToolCallRevertFile as <- grp])
          ]
        ToolCallSummariseAction _ ->
          [ ToolCallSummariseAction
              (concat [as | ToolCallSummariseAction as <- grp])
          ]
        ToolCallFileLineOp _ ->
          [ ToolCallFileLineOp
              (concat [as | ToolCallFileLineOp as <- grp])
          ]
        ToolCallAddDependency _ ->
          [ ToolCallAddDependency
              (concat [as | ToolCallAddDependency as <- grp])
          ]
        -- We do not merge these, so just leave them untouched:
        ToolCallPanic _ -> grp
        ToolCallEscalate _ -> grp
        ToolCallReturn _ -> grp

getFileName :: Object -> Maybe String
getFileName obj =
  case KM.lookup "fileName" obj of
    Just (String textValue) ->
      Just (T.unpack textValue)
    _ -> Nothing

getAllFileNames :: [Object] -> [Text]
getAllFileNames objects =
  let fileNames = mapMaybe getFileName objects
   in map T.pack $ Set.toList (Set.fromList fileNames)

getToolCallFileNames :: [(Tool, [AET.Object])] -> [Text]
getToolCallFileNames vals =
  let names = concatMap (\(_, objs) -> getAllFileNames objs) vals
   in Set.toList $ Set.fromList names

data OpenFileArg = OpenFileArg
  { fileName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON OpenFileArg

instance FromJSON OpenFileArg

data FocusFileArg = FocusFileArg
  { fileName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON FocusFileArg

instance FromJSON FocusFileArg

data CloseFileArg = CloseFileArg
  { fileName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON CloseFileArg

instance FromJSON CloseFileArg

data AppendFileArg = AppendFileArg
  { fileName :: Text,
    rawTextName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON AppendFileArg

instance FromJSON AppendFileArg

data EditFileArg = EditFileArg
  { fileName :: Text,
    startLineNum :: Int,
    endLineNum :: Int,
    rawTextName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON EditFileArg

instance FromJSON EditFileArg

data EditFileByMatchArg = EditFileByMatchArg
  { fileName :: Text,
    startLineMatchesRegex :: Text,
    startClosestToLineNum :: Int,
    endLineMatchesRegex :: Text,
    endClosestToLineNum :: Int,
    rawTextName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON EditFileByMatchArg

instance FromJSON EditFileByMatchArg

data InsertInFileArg = InsertInFileArg
  { fileName :: Text,
    lineNum :: Int,
    rawTextName :: Text
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

data FileLineOpArg = FileLineOpArg
  { fileName :: Text,
    startLineNum :: Int,
    endLineNum :: Int,
    rawTextName :: Text,
    origToolName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON FileLineOpArg

instance FromJSON FileLineOpArg

data AddDependencyArg = AddDependencyArg
  { dependencyName :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON AddDependencyArg
instance FromJSON AddDependencyArg

-- Note: reverse sort so later line numbers are modified first,
-- to avoid the meaning of line numbers changing
validateAndSortFileLineArgs :: [FileLineOpArg] -> Either Text [FileLineOpArg]
validateAndSortFileLineArgs args = do
  -- First validate all line numbers
  validateFileLineOpLineNumbers args
  -- Then check for overlaps within each file
  validateFileLineOpNoOverlaps args
  -- If all validations pass, return sorted list
  Right $ sortBy compareFileLineOpArgs args

-- Compare function for sorting
compareFileLineOpArgs :: FileLineOpArg -> FileLineOpArg -> Ordering
compareFileLineOpArgs a b = case compare a.fileName b.fileName of
  EQ -> compare b.startLineNum a.startLineNum
  other -> other

-- Validate individual line numbers
validateFileLineOpLineNumbers :: [FileLineOpArg] -> Either Text ()
validateFileLineOpLineNumbers args =
  case filter invalidLineNumbers args of
    [] -> Right ()
    (arg : _) ->
      Left
        $ T.concat
          [ "Invalid line numbers for <> " <> arg.origToolName <> ": ",
            arg.fileName,
            " start: ",
            T.pack (show arg.startLineNum),
            " end: ",
            T.pack (show arg.endLineNum)
          ]
  where
    invalidLineNumbers arg =
      arg.startLineNum
        < 0
        || arg.endLineNum
        < 0
        || arg.endLineNum
        < arg.startLineNum

-- Validate no overlapping ranges within same file
validateFileLineOpNoOverlaps :: [FileLineOpArg] -> Either Text ()
validateFileLineOpNoOverlaps args =
  let groupedByFile =
        L.groupBy (\a b -> a.fileName == b.fileName)
          $ sortBy compareFileLineOpArgs args
   in case findFirstFileLineOpOverlap groupedByFile of
        Nothing -> Right ()
        Just (a, b) ->
          Left
            $ T.concat
              [ "Overlapping ranges in file: ",
                a.fileName,
                " between " <> a.origToolName <> " ",
                T.pack (show (a.startLineNum, a.endLineNum)),
                " and " <> b.origToolName <> " ",
                T.pack (show (b.startLineNum, b.endLineNum))
              ]

-- Helper to find first overlap in sorted groups
findFirstFileLineOpOverlap :: [[FileLineOpArg]] -> Maybe (FileLineOpArg, FileLineOpArg)
findFirstFileLineOpOverlap = getFirst . map findOverlapInGroup
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
      a.startLineNum
        < b.endLineNum
        && a.endLineNum
        >= b.startLineNum

data PanicArg = PanicArg
  { reason :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON PanicArg
instance FromJSON PanicArg

data EscalateArg = EscalateArg
  { escalationReason :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON EscalateArg
instance FromJSON EscalateArg

toolName :: Tool -> Text
toolName x = T.drop 4 $ show x

toolSummary :: Text
toolSummary = "You have the following tools available to you, that you may call with JSON args. Line numbers are included for your focused OpenFiles to simplify your task, but are not present in the files on disk (so don't explicitly write line numbers to disk!). Only a limited number of source files can be 'focused' (shown in full detail) at a time; to avoid overwhelming the context, the rest will only show function types and struct definitions. You may append to an open file that's not focused, but not do any line-number-based edits (as you can't see function bodies in an unfocused file). For tools that modify files, after modification the file will be compiled if a source file, and run if it's a unit test file. \n IMPORTANT NOTE: for Append/Edit/InsertIn file, you don't provide the text as part of the json, instead you set \"rawTextName\": \"someRawTextBox\", and then afterwards include the raw string literal in C++ style RAWTEXT[someRawTextBox]=R\"r( ...the actual text... )r\". This is to avoid the need for JSON-escaping the code/text; you instead directly include the unescaped text in between the R\"r( and )r\". It allows allows multiple commands to refer to the same raw text box where necessary (e.g. if inserting the same code in multiple places). Note that LINE NUMBERS START AT ZERO, and appear at the START of the line, not the end. Also PLEASE NOTE: you can't make overlapping line-based edits, as order of operations for overlapping edits is undefined. Upon any change to a file, the build system will run automatically (e.g. go generate -> go build -> go fmt -> go test, or equivalents for whatever language the project is in).\nNOTE: line number comments like /* 10 */ that you may see in open files are purely for your reference, and are _not_ present in the file on disk, so don't reference these in your tool calls."

-- | Compile a Text regex with error reporting using regex-tdfa.
compileRegex :: Text -> Either Text Regex
compileRegex pat =
  first
    ( \errStr ->
        "Invalid regex '" <> pat <> "': " <> errStr
    )
    $ makeRegexM (T.unpack pat)

-- | Check if a Text line matches a compiled Regex.
lineMatches :: Regex -> Text -> Bool
lineMatches = match

-- | Find the integer in a non-empty list closest to the target.
--   If there's a tie, it will choose the smaller line number.
closestTo :: Int -> [Int] -> Maybe Int
closestTo target xs = viaNonEmpty Relude.head $ sortOn (\n -> (abs (n - target), n)) xs

-------------------------------------------------------------------------------

-- | The main function.
--   Takes (startRegex, startClosestLine) and (endRegex, endClosestLine), plus the file/text content.
--   Returns either an error or (startLineNum, endLineNum).

-------------------------------------------------------------------------------
getLineNumsFromRegex ::
  -- | (startLineNumRegex, startLineNumClosestTo)
  (Text, Int) ->
  -- | (endLineNumRegex,   endLineNumClosestTo)
  (Text, Int) ->
  -- | The text to scan
  Text ->
  Either Text (Int, Int)
getLineNumsFromRegex
  (startLineNumRegex, startLineNumClosestTo)
  (endLineNumRegex, endLineNumClosestTo)
  txt = do
    -- 1. Compile both regexes, or fail with descriptive error
    startRegex <- compileRegex startLineNumRegex
    endRegex <- compileRegex endLineNumRegex

    let linesList = T.lines txt
        -- Attach 0-based line numbers
        numberedLines = zip [0 ..] linesList

        -- 2. All lines matching the start pattern
        matchedStartLines =
          [ i
          | (i, lineText) <- numberedLines,
            lineMatches startRegex lineText
          ]
        errEnd = "Note only POSIX character classes are supported (e.g. [:space:] not \\s). Try to use the simplest matching regex possible to minimise mistakes, and only use ^ and $ start/end matching when absolutely necessary to disambiguate."

    -- 3. Pick the match closest to startLineNumClosestTo (if none, error)
    startLineNum <-
      maybeToRight
        ( "No lines matched the start pattern '"
            <> startLineNumRegex
            <> "' in the entire text. "
            <> errEnd
        )
        (closestTo startLineNumClosestTo matchedStartLines)

    -- 4. Among lines >= startLineNum, match the end pattern
    let matchedEndLines =
          [ i
          | (i, lineText) <- numberedLines,
            i >= startLineNum,
            lineMatches endRegex lineText
          ]

    -- 5. Pick the match closest to endLineNumClosestTo (if none, error)
    endLineNum <-
      maybeToRight
        ( "No lines matched the end pattern '"
            <> endLineNumRegex
            <> "' at or after line "
            <> show startLineNum
            <> ". "
            <> errEnd
        )
        (closestTo endLineNumClosestTo matchedEndLines)

    -- 6. Return success
    pure (startLineNum, endLineNum)

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

mkSampleCodeBox :: Text -> Text
mkSampleCodeBox name = "\nRAWTEXT[" <> name <> "]=R\"r( someCodeHere()\n someMoreCodeHere)r\""

-- Returns arg format json, rawTextBoxExample, description
toolArgFormatAndDesc :: Tool -> (Text, Text, Text)
toolArgFormatAndDesc ToolReturn = ("{ }", "", "Return a value; format depends on the task and is described further down below. Where the return references a change made to a file, it should only be returned _after_ the change is made. Note you can only return a single value at a time!")
toolArgFormatAndDesc ToolFileLineOp = (toJ FileLineOpArg {fileName = "somefile.txt", startLineNum = 5, endLineNum = 10, rawTextName = "codeBoxToUse", origToolName = "originalToolName"}, mkSampleCodeBox "codeBoxToUse", "You should panic if you see this; it's an internal tool that insert/edit are transformed into, and you shouldn't call it directly.")
toolArgFormatAndDesc ToolOpenFile = (toJ OpenFileArg {fileName = "someFile.txt"}, "", "Load a file into the context")
toolArgFormatAndDesc ToolFocusFile = (toJ FocusFileArg {fileName = "someFile.txt"}, "", "Focus on an unfocused source file in the context, showing in full detail (as opposed to just struct defs and function headers). This is necessary for editing a file by line. Note that if already at the max number of focused files, this will cause the least recently modified focused file to be unfocused. Does nothing for non-code files.")
toolArgFormatAndDesc ToolCloseFile = (toJ CloseFileArg {fileName = "someFile.txt"}, "", "Remove a file from the context")
toolArgFormatAndDesc ToolAppendFile = (toJ AppendFileArg {fileName = "somefile.txt", rawTextName = "codeToAppendBox"}, mkSampleCodeBox "codeToAppendBox", "Append code/text to the bottom of a file. Can be used to create a new file if the file doesn't exist, and can be done to a non-focused file.")
toolArgFormatAndDesc ToolReplaceFile = (toJ AppendFileArg {fileName = "somefile.txt", rawTextName = "codeToReplaceBox"}, mkSampleCodeBox "codeToReplaceBox", "Replace a file with the provided code/text to a file. Can be used to create a new file. Prefer this over editing when the file is small.")
toolArgFormatAndDesc ToolEditFile = (toJ EditFileArg {fileName = "somefile.txt", startLineNum = 5, endLineNum = 10, rawTextName = "codeBoxToReplaceWith"}, mkSampleCodeBox "codeBoxToReplaceWith", "Replace text in [startLineNum, endLineNum] with the text you provide. Note if making multiple edits to the same file, the start/end line numbers of different edits cannot overlap. IMPORTANT: if you insert more lines than you're replacing, the rest will be inserted, not replaced. So inserting 2 lines at at startLineNum=15 endLineNum=15 will only replace the existing line 15 in the file, and add the second provided line after that, it won't replace lines 15 and 16. Note too that the line-numbers are provided to you at the START of the line in every file. Remember line numbers start at zero, and that multiple edits cannot overlap!")
toolArgFormatAndDesc ToolEditFileByMatch = (toJ EditFileByMatchArg {fileName = "somefile.txt", startLineMatchesRegex = "int[[:space:]]*some_func(.*){", startClosestToLineNum = 5, endLineMatchesRegex = "^}", endClosestToLineNum = 20, rawTextName = "codeBoxToReplaceWith"}, mkSampleCodeBox "codeBoxToReplaceWith", "Finds lines matching the startLineNumMatchesRegex and endLineMatchesRegex, and replaces them and the lines between them with with the text you provide. Where multiple matches are present, the match closest to startClosestToLineNum/endClosestToLineNum will be used. Note if making multiple edits to the same file, the regions edited cannot overlap. Note also the regex is simple DFA, and does not support fancy PCRE features, or character classes like \\s, only posix classes like [:space:] are supported. Multi-line regex are NOT supported; startLineMatchesRegex and endLineMatchesRegex should be simple single-line regexes that match the first and last line to replace respectively.\nNOTE AGAIN that the linenum comments like /* lineNum */ are purely to assist you and not present on disk, so your regex shouldn't assume they exist. Remember multiple edits cannot overlap!")
toolArgFormatAndDesc ToolInsertInFile = (toJ InsertInFileArg {fileName = "somefile.txt", lineNum = 17, rawTextName = "codeToInsertBox"}, mkSampleCodeBox "codeToInsertBox", "Insert the provided text into the file at lineNum, not replacing/overwriting the content on that line (instead it's moved to below the inserted text). Note that this cannot overlap with lines modified by an Edit tool, as order of operations is undefined.")
toolArgFormatAndDesc ToolAddDependency = (toJ AddDependencyArg {dependencyName = "github.com/xitongsys/parquet-go"}, "", "Add the dependency in the appropriate manner for the build system. E.g. golang will go get dependencyName, Rust will cargo add dependencyName.")
toolArgFormatAndDesc ToolRevertFile = (toJ RevertFileArg {fileName = "someFile.txt"}, "", "Revert un-added changes in an open file; changes are committed when compilation and unit tests succeed, so will revert to the last version of the file before compilation or unit tests failed. Use this if you get the file in a state you can't recover it from.")
toolArgFormatAndDesc ToolPanic = (toJ PanicArg {reason = "This task is impossible for me to do because ..."}, "", "Call this if you can't complete the task due to it being impossible or not having enough information. Also call this if the task needs a tool that you don't have.")
toolArgFormatAndDesc ToolEscalate = (toJ PanicArg {reason = "I can't solve the compilation error _____ by myself, I've tried approaches foo, blah and blaz but nothing helped"}, "", "Call this if you need help with the task and are unable to do it yourself, to request help from a human or more powerful model.")
toolArgFormatAndDesc ToolSummariseAction = (toJ SummariseActionArg {  actionSummary = "Adding the field blah to someType",
  actionReason = "The spec requires someType to have that field",
  actionFuturePlanSummary = "Implement functionality blaz that requires blah field on someType"}, "", "Call this once in every response that's not just a plain return, to allow keeping a record of your actions and future plans, to keep on track and avoid getting caught in cycles. Please keep the summary as brief as possible.")

mkToolCallSyntax :: Tool -> Text -> Text
mkToolCallSyntax tool argFormat = toolName tool <> "=<[" <> argFormat <> "]>"

toolToDescription :: Tool -> Text
toolToDescription x = do
  let (argFormat, rawTextFormat, toolDesc) = toolArgFormatAndDesc x
  "Name:" <> toolName x <> "\nSyntax: " <> mkToolCallSyntax x argFormat <> rawTextFormat <> "\nDescription: " <> toolDesc <> "\n"

returnValueToDescription :: (ToJSON a) => a -> Text
returnValueToDescription example = do
  let exampleTxt = toJ example
  let fmt = "You must return the output in a format like the following: " <> mkToolCallSyntax ToolReturn exampleTxt
  fmt <> " \n You must either return a value or call a tool. Because you're part of an automated process, you cannot prompt the user for information, so panic if you don't know how to proceed. Please do NOT use ```json syntax, as it's not supported."

toolsToDescription :: [Tool] -> Text
toolsToDescription tools = toolSummary <> "\nAll available tools: \n" <> T.unlines (map toolToDescription tools) <> "\n Multiple tool calls are supported, you can either do ToolName=<[{jsonArgs}]>, ToolName=<[{otherJsonArgs}]>, or ToolName=<[{jsonArgs}, {otherJsonArgs}]>; both are supported. (Replace ToolName here with the actual name of a tool; ToolName itself is not a tool!). Remember that the latest state of the files to modify is in the OpenFiles section. Please include one SummariseAction call in each response to capture your intent, and try to limit the number of file edits/modifications per response, to minimise the number of potential compilation errors/unit test failures that need to be fixed at once."

tmp :: Text
tmp = "AppendFile<[{\"fileName\":\"websocket_client.h\",\"text\":\"#pragma once\\n\\n#include <libwebsockets.h>\\n#include \\\"config.h\\\"\\n#include \\\"simdjson.h\\\"\\n#include \\\"book_data.h\\\"\\n#include \\\"trade_data.h\\\"\\n#include <spdlog/spdlog.h>\\n#include <functional>\\n\\nnamespace websocket {\\n\\nstruct Handler {\\n    virtual void on_trade(const trade_data::TradeEvent& trade) = 0;\\n    virtual void on_agg_trade(const trade_data::AggTradeEvent& agg_trade) = 0;\\n    virtual void on_book_update(const book_data::BookUpdate& update) = 0;\\n    virtual void on_best_bid_ask(const book_data::BestBidAsk& update) = 0;\\n    virtual void request_snapshot(const std::string& symbol) = 0;\\n    virtual ~Handler() = default;\\n};\\n\\nnamespace core {\\n    bool check_sequence_gap(uint64_t last_update_id, const book_data::BookUpdate& update);\\n    void process_message(simdjson::ondemand::document& doc, Handler& handler);\\n}\\n\\nclass WebSocketClient {\\npublic:\\n    WebSocketClient(config::BinanceConfig config, Handler& handler);\\n    ~WebSocketClient();\\n\\n    void connect();\\n    void poll(int timeout_ms = 0);\\n\\nprivate:\\n    static int lws_callback(lws* wsi, lws_callback_reasons reason, void* user, void* in, size_t len);\\n    int handle_callback(lws* wsi, lws_callback_reasons reason, void* in, size_t len);\\n\\n    lws_context* context = nullptr;\\n    lws* wsi = nullptr;\\n    config::BinanceConfig config;\\n    Handler& handler;\\n    simdjson::ondemand::parser json_parser;\\n};\\n\\n} // namespace websocket\\n\"}]>\nOpenFile=<[{\"fileName\":\"websocket_client.h\"}]>\n\nReturn=<[{\"createdFiles\":[{\"createdFileName\":\"websocket_client.h\",\"createdFileSummary\":\"Libwebsockets wrapper for Binance with message processing core. Handles WS connection lifecycle, message parsing using simdjson, sequence gap detection, and event dispatch to handler interfaces. Separates pure message validation (check_sequence_gap) from IO-bound WS ops. Uses config::BinanceConfig for endpoints and symbols. Pure core logic in namespace allows testing without live connection.\"}]}]>"

type RawTexts = [(Text, Text)]

--   RAWTEXT[someName]=[R| ...contents... |R] or RAWTEXT[someName]=R"r( ...contents... )r"
-- Returns a list of (someName, contents) pairs.
extractRawStrings :: Text -> Either Text [(Text, Text)]
extractRawStrings = parseRawTexts

-- | Parse raw text entries from a Text
parseRawTexts2 :: Text -> [(Text, Text)]
parseRawTexts2 input = go input []
  where
    go :: Text -> [(Text, Text)] -> [(Text, Text)]
    go txt acc
      | T.null txt = reverse acc
      | otherwise = case findRawTextStart txt of
          Nothing -> reverse acc
          Just (name, rest) ->
            case findRawTextContent rest of
              Nothing -> reverse acc
              Just (content, remaining) ->
                go remaining ((name, content) : acc)

    -- \| Find the start of a RAWTEXT entry and extract the name
    findRawTextStart :: Text -> Maybe (Text, Text)
    findRawTextStart txt = do
      -- Look for "RAWTEXT["
      (_, afterKeyword) <- breakOnMaybe (T.pack "RAWTEXT[") txt
      -- Extract the name and find the closing bracket
      (name, afterName) <- extractBetween ']' afterKeyword
      -- Look for "=[R|" marker
      (_, afterMarker) <- breakOnMaybe (T.pack "=[R|") afterName
      return (name, afterMarker)

    -- \| Find the content between [R| and |R]
    findRawTextContent :: Text -> Maybe (Text, Text)
    findRawTextContent txt = do
      -- Find the |R] marker, everything before it is content
      (content, afterContent) <- breakOnMaybe (T.pack "|R]") txt
      return (content, afterContent)

    -- \| Break a Text on a substring, returning Nothing if not found
    breakOnMaybe :: Text -> Text -> Maybe (Text, Text)
    breakOnMaybe sub txt =
      let (before, after) = T.breakOn sub txt
       in if T.null after then Nothing else Just (before, T.drop (T.length sub) after)

    -- \| Extract text between current position and a closing character
    extractBetween :: Char -> Text -> Maybe (Text, Text)
    extractBetween close txt =
      let (content, remaining) = T.break (== close) txt
       in if T.null remaining
            then Nothing
            else Just (content, T.drop 1 remaining)

parseRawTexts :: Text -> Either Text [(Text, Text)]
parseRawTexts input = go input []
  where
    -- The main worker function. We accumulate results in 'acc' as we go.
    go :: Text -> [(Text, Text)] -> Either Text [(Text, Text)]
    go txt acc
      | T.null txt = Right (reverse acc)
      | otherwise =
          case findRawTextStart txt of
            Left noStartMsg
              -- If we cannot find "RAWTEXT[" at all, we are done; return accumulated.
              | noStartMsg == "No RAWTEXT found" -> Right (reverse acc)
              | otherwise -> Left noStartMsg
            Right (name, closeMarker, afterMarker) -> do
              (content, remaining) <- findRawTextContent name closeMarker afterMarker
              -- Successfully parsed a block. Continue with the remainder.
              go remaining ((name, content) : acc)

    ------------------------------------------------------------
    -- 1. Find the start of a RAWTEXT entry ("RAWTEXT[...]")
    --    and figure out which opening marker we have: =[R| or =R"r(.
    ------------------------------------------------------------
    findRawTextStart :: Text -> Either Text (Text, Text, Text)
    findRawTextStart txt = do
      -- We first look for "RAWTEXT["
      afterKeyword <- dropUpToSubstring "RAWTEXT[" "No RAWTEXT found" txt
      -- Next we parse the name up to the closing bracket ']'
      (name, afterName) <- extractBetween ']' "Missing ']' after RAWTEXT name" afterKeyword
      -- Now we expect either "=[R|" or "=[R>" right after the name
      case breakOnEither "=[R|" afterName of
        Right (_, afterMarker) -> Right (name, "|R]", afterMarker)
        Left _ ->
          case breakOnEither "=R\"r(" afterName of
            Right (_, afterMarker) -> Right (name, ")r\"", afterMarker)
            Left _ -> Left $ "Missing opening marker '=[R|' or '=R\"r(' after name: " <> name <> ", when attempting to extract it from: " <> afterName

    ------------------------------------------------------------
    -- 2. Given a closeMarker ('|R]' or ')r"'), find the text
    --    until that closeMarker, and return (content, remainder).
    ------------------------------------------------------------
    findRawTextContent :: Text -> Text -> Text -> Either Text (Text, Text)
    findRawTextContent blockName closeMarker txt = do
      (content, afterContent) <-
        breakOnEither closeMarker txt
          `orElse` ("Could not find matching close marker " <> closeMarker <> " for " <> blockName)
      return (content, afterContent)

    ------------------------------------------------------------
    -- Utility: break a Text on a substring, returning Right (before, after)
    -- if found, or Left if not found.
    ------------------------------------------------------------
    breakOnEither :: Text -> Text -> Either Text (Text, Text)
    breakOnEither sub t =
      let (before, after) = T.breakOn sub t
       in if T.null after
            then Left ("Substring '" <> sub <> "' not found")
            else Right (before, T.drop (T.length sub) after)

    ------------------------------------------------------------
    -- Utility: skip everything up to a given substring, returning the text
    -- after that substring. If not found, return a custom error.
    ------------------------------------------------------------
    dropUpToSubstring :: Text -> Text -> Text -> Either Text Text
    dropUpToSubstring sub err t =
      case breakOnEither sub t of
        Left _ -> Left err
        Right (_, after) -> Right after

    ------------------------------------------------------------
    -- Utility: extractBetween closeChar errMsg text tries to break on 'closeChar'
    -- and returns (content, afterCloseChar).
    ------------------------------------------------------------
    extractBetween :: Char -> Text -> Text -> Either Text (Text, Text)
    extractBetween closeChar errMsg t =
      let (content, remaining) = T.break (== closeChar) t
       in if T.null remaining
            then Left errMsg
            else Right (content, T.drop 1 remaining)

    ------------------------------------------------------------
    -- Utility: a small combinator to replace Left errors with a new message.
    ------------------------------------------------------------
    orElse :: Either a b -> a -> Either a b
    orElse (Left _) e = Left e
    orElse (Right x) _ = Right x

tmpp :: Text
tmpp = "EditFileByMatch=<[{\n  \"fileName\": \"binanceFuturesSubscriber_test.go\",\n  \"startClosestToLineNum\": 273,\n  \"startLineMatchesRegex\": \"^// Test that a LevelNewQty message has the Side field set\",\n  \"endClosestToLineNum\": 285,\n  \"endLineMatchesRegex\": \"^\\\\s*}\\\\s*$\",\n  \"rawTextName\": \"replaceLevelNewQtyBlock\"\n}]>\nRAWTEXT[replaceLevelNewQtyBlock]=R\"r(\t// Test that a LevelNewQty message has the Side field set by using the helper function.\n\tdummyLevelNewQty := NewLevelNewQty(\"binance_cfutures\", \"BTCUSDT\", 0, 10000.5, 5, 1, 1630001000, 1630001001, \"\")\n\tif dummyLevelNewQty.Side == \"\" {\n\t\tt.Errorf(\"Side field not set in LevelNewQty message\")\n\t}\n)r\"\n  \nReturn=<[{\"fileFixConfirmed\": true, \"rationale\": \"Changed the test in TestAddedFieldsFutures to use the NewLevelNewQty constructor so that a missing side defaults to 'bid'. This ensures that the LevelNewQty message has a non-empty Side field as expected by the specification.\"}]>"

findJsonContentInBlocks :: Bool -> Text -> [Text]
findJsonContentInBlocks mustHaveJson text = go text []
  where
    go :: Text -> [Text] -> [Text]
    go currentText acc =
      let initiator = if mustHaveJson then "```json" else "```" in
      case T.breakOn initiator currentText of
        (_, "") -> reverse acc -- No more "```json" start markers found
        (_beforeBlock, textStartingWithMarker) ->
          let textAfterMarker = T.drop (T.length initiator) textStartingWithMarker
          in case T.breakOn "```" textAfterMarker of
               (_, "") -> reverse acc -- No closing "```" found for the last block (malformed or end of input)
               (jsonContent, textAfterClosingMarker) ->
                 -- jsonContent is the raw text between "```json" and the next "```"
                 -- textAfterClosingMarker starts with "```" followed by the rest of the input
                 let nextSearchText = T.drop (T.length "```") textAfterClosingMarker
                 in go nextSearchText (jsonContent : acc)

tmppGem :: Text
tmppGem = "```json\n[\n  {\n    \"tool_name\": \"EditFileByMatch\",\n    \"args\": {\n      \"fileName\": \"internal/features/label_cursor.go\",\n      \"startClosestToLineNum\": 359,\n      \"startLineMatchesRegex\": \"[[:space:]]*labelF16 := float16.Float16\\\\(labelValue64\\\\)\",\n      \"endClosestToLineNum\": 360,\n      \"endLineMatchesRegex\": \"[[:space:]]*labelF16 := float16.Float16\\\\(labelValue64\\\\)\",\n      \"rawTextName\": \"correct_float16_conversion_1\"\n    }\n  },\n  {\n    \"tool_name\": \"EditFileByMatch\",\n    \"args\": {\n      \"fileName\": \"internal/features/label_cursor.go\",\n      \"startClosestToLineNum\": 366,\n      \"startLineMatchesRegex\": \"[[:space:]]*tempMap\\\\[instrument\\\\] = float16.Float16\\\\(0.0\\\\) // Assign 0.0 label\",\n      \"endClosestToLineNum\": 367,\n      \"endLineMatchesRegex\": \"[[:space:]]*tempMap\\\\[instrument\\\\] = float16.Float16\\\\(0.0\\\\) // Assign 0.0 label\",\n      \"rawTextName\": \"correct_float16_conversion_0\"\n    }\n  },\n  {\n    \"tool_name\": \"EditFileByMatch\",\n    \"args\": {\n      \"fileName\": \"internal/features/label_cursor.go\",\n      \"startClosestToLineNum\": 325,\n      \"startLineMatchesRegex\": \"[[:space:]]*tempMap\\\\[instrument\\\\] = float16.Float16\\\\(0.0\\\\) // Assign 0.0 to temp map\",\n      \"endClosestToLineNum\": 326,\n      \"endLineMatchesRegex\": \"[[:space:]]*tempMap\\\\[instrument\\\\] = float16.Float16\\\\(0.0\\\\) // Assign 0.0 to temp map\",\n      \"rawTextName\": \"correct_float16_conversion_0\"\n    }\n  },\n  {\n    \"tool_name\": \"SummariseAction\",\n    \"args\": {\n      \"actionSummary\": \"Correct float64 to float16 conversion in MidPriceMoveLabelCursor.Advance by using float16.Fromfloat32.\",\n      \"actionReason\": \"The direct cast `float16.Float16(float64_val)` was incorrect, leading to wrong float16 bit patterns (e.g., 1.0 becoming 0x0001 instead of 0x3C00). This caused test failures where labels were not matching expected values.\",\n      \"actionFuturePlanSummary\": \"Re-run tests. If label cursor tests still fail, investigate mock setups for BookManager in featureCursorState and the labelCursor's internalBookManager, as well as the logic for populating `midPriceAtT`.\"\n    }\n  }\n]\n```"

tmppGem2 :: Text
tmppGem2 = "```json\n[\n  {\n    \"tool_name\": \"FocusFile\",\n    \"args\": [\n      {\n        \"fileName\": \"cmd/marketdataserver/main.go\"\n      }\n    ]\n  },\n  {\n    \"tool_name\": \"FocusFile\",\n    \"args\": [\n      {\n        \"fileName\": \"cmd/marketdataserver/main_test.go\"\n      }\n    ]\n  },\n  {\n    \"tool_name\": \"EditFileByMatch\",\n    \"args\": [\n      {\n        \"fileName\": \"cmd/marketdataserver/main.go\",\n        \"startClosestToLineNum\": 196,\n        \"startLineMatchesRegex\": \"func startReaderManager\\\\(ctx context.Context, appWg \\\\*sync.WaitGroup, cfg \\\\*config.Config, mdp metadata.MetadataProvider, logger \\\\*slog.Logger, rFactory readerFactory\\\\) \\\\[\\\\]<-chan \\\\*types.MarketEvent {\",\n        \"endClosestToLineNum\": 199,\n        \"endLineMatchesRegex\": \"func startReaderManager\\\\(ctx context.Context, appWg \\\\*sync.WaitGroup, cfg \\\\*config.Config, mdp metadata.MetadataProvider, logger \\\\*slog.Logger, rFactory readerFactory\\\\) \\\\[\\\\]<-chan \\\\*types.MarketEvent {\",\n        \"rawTextName\": \"startReaderManagerSignatureChange\"\n      }\n    ]\n  },\n  {\n    \"tool_name\": \"InsertInFile\",\n    \"args\": [\n      {\n        \"fileName\": \"cmd/marketdataserver/main.go\",\n        \"lineNum\": 213,\n        \"rawTextName\": \"usePredefinedLogic\"\n      }\n    ]\n  },\n  {\n    \"tool_name\": \"EditFileByMatch\",\n    \"args\": [\n      {\n        \"fileName\": \"cmd/marketdataserver/main.go\",\n        \"startClosestToLineNum\": 264,\n        \"startLineMatchesRegex\": \"r, err := rFactory\\\\(sourceCfg, cfg, mdp\\\\) // Use the passed factory\",\n        \"endClosestToLineNum\": 271,\n        \"endLineMatchesRegex\": \"continue\",\n        \"rawTextName\": \"readerInstantiationLogic\"\n      }\n    ]\n  },\n  {\n    \"tool_name\": \"EditFileByMatch\",\n    \"args\": [\n      {\n        \"fileName\": \"cmd/marketdataserver/main.go\",\n        \"startClosestToLineNum\": 103,\n        \"startLineMatchesRegex\": \"readerOutputChans := startReaderManager\\\\(mainCtx, &appWg, cfg, mdp, appLogger, reader.NewReader\\\\)\",\n        \"endClosestToLineNum\": 103,\n        \"endLineMatchesRegex\": \"readerOutputChans := startReaderManager\\\\(mainCtx, &appWg, cfg, mdp, appLogger, reader.NewReader\\\\)\",\n        \"rawTextName\": \"runApplicationCallSiteUpdate\"\n      }\n    ]\n  },\n  {\n    \"tool_name\": \"EditFileByMatch\",\n    \"args\": [\n      {\n        \"fileName\": \"cmd/marketdataserver/main_test.go\",\n        \"startClosestToLineNum\": 419,\n        \"startLineMatchesRegex\": \"// Define the mock reader factory for this test\",\n        \"endClosestToLineNum\": 486,\n        \"endLineMatchesRegex\": \"readerOutputChans := startReaderManager\\\\(mainAppCtx, &internalAppWg, cfg, mdp, util.Log, mockReaderFactory\\\\)\",\n        \"rawTextName\": \"testMockReaderInstantiationAndUpdateCall\"\n      }\n    ]\n  },\n  {\n    \"tool_name\": \"SummariseAction\",\n    \"args\": [\n      {\n        \"actionSummary\": \"Modified `startReaderManager` to accept pre-instantiated readers and updated `main.go` and `main_test.go` accordingly.\",\n        \"actionReason\": \"The task required allowing tests to inject mock readers more directly into `startReaderManager`.\",\n        \"actionFuturePlanSummary\": \"Verify test pass and ensure no regressions.\"\n      }\n    ]\n  }\n]\n```"

tmppGem3 :: Text
tmppGem3 = "The tests are still failing with timeouts in `TestRouting_MatchingLogic`.\nThe `StaticFileServer.ServeHTTP2` method logs show it attempts to send headers and data.\nThe `Stream.SendHeaders` and `Stream.WriteData` methods also log their entry.\nHowever, the `conn.sendHeadersFrame` and `conn.sendDataFrame` are still not being logged as called by the `Stream` methods.\n\nThe code inserted for `Stream.WriteData` has a subtle bug:\n`c.settingsMu.RLock()`\n`peerMaxFrameSize := c.peerMaxFrameSize`\n`c.settingsMu.RUnlock()`\n\n`c` is not defined within `Stream.WriteData`. It should be `s.conn`.\n\nLet's fix this.```\n[\n  {\n    \"tool_name\": \"EditFileByMatch\",\n    \"args\": {\n      \"fileName\": \"internal/http2/stream.go\",\n      \"startClosestToLineNum\": 199,\n      \"startLineMatchesRegex\": \"\\\\s*c\\\\.settingsMu\\\\.RLock\\\\(\\\\)\",\n      \"endClosestToLineNum\": 201,\n      \"endLineMatchesRegex\": \"\\\\s*c\\\\.settingsMu\\\\.RUnlock\\\\(\\\\)\",\n      \"rawTextName\": \"stream_writedata_conn_fix\"\n    }\n  },\n  {\n    \"tool_name\": \"SummariseAction\",\n    \"args\": {\n      \"actionSummary\": \"Correct `Stream.WriteData` to use `s.conn` instead of `c` when accessing connection settings.\",\n      \"actionReason\": \"The previous implementation of `Stream.WriteData` incorrectly used `c.settingsMu` and `c.peerMaxFrameSize` where `c` was undefined. It should have been `s.conn.settingsMu` and `s.conn.peerMaxFrameSize`. This caused a compile error, which implicitly prevented the intended logic for checking `peerMaxFrameSize` from running.\",\n      \"actionFuturePlanSummary\": \"Re-evaluate test logs. If `conn.sendDataFrame` is still not called, investigate other parts of `Stream.WriteData`, particularly flow control logic (even if stubbed) or any other conditions that might prevent the call to `s.conn.sendDataFrame`.\"\n    }\n  }\n]\nRAWTEXT[stream_writedata_conn_fix]=R\"r(\n\ts.conn.settingsMu.RLock() // Use RLock as we are only reading peerMaxFrameSize\n\tpeerMaxFrameSize := s.conn.peerMaxFrameSize\n\ts.conn.settingsMu.RUnlock()\n)r\"\n```"

removeRawTextLiterals :: String -> String
removeRawTextLiterals input =
    let
        -- Regex explanation:
        -- RAWTEXT          : Literal "RAWTEXT"
        -- \\[             : Literal "[" (escaped for regex)
        -- .*             : Any character (except newline by default) zero or more times (for content inside [])
        -- \\]             : Literal "]" (escaped for regex)
        -- =                : Literal "="
        -- R"r             : Literal "R\"r"
        -- ([\\s\\S]*?)    : Capture group 1:
        --   [\\s\\S]     : Any whitespace OR non-whitespace character (i.e., any char including newline)
        --   *?           : Zero or more times, non-greedy (important to stop at the first "r\"")
        -- r"              : Literal "r\""
        --
        -- Note: In Haskell string literals, '\' is an escape char, so '\\' becomes a literal '\' for the regex engine.
        regexPattern :: String
        regexPattern = "RAWTEXT\\[.*\\]=R\"r\\((.|\n)*\\)r\""

        regex = mkRegex regexPattern

    in
        -- subRegex replaces all non-overlapping matches of 'regex' in 'input'
        -- with the replacement string (here, an empty string "").
        subRegex regex input ""

extractJsonToolCallsGeminiFormat :: Text -> Either Text (M.Map T.Text [AET.Object])
extractJsonToolCallsGeminiFormat fullText =
  let jsonContentStringsAttempt1 = findJsonContentInBlocks True fullText
      jsonContentStrings = map (T.pack . removeRawTextLiterals . T.unpack) $ if null jsonContentStringsAttempt1 then findJsonContentInBlocks False fullText else jsonContentStringsAttempt1

      parseAndCollectBlocks :: [Text] -> Either Text (M.Map T.Text [AET.Object])
      parseAndCollectBlocks [] = Right M.empty
      parseAndCollectBlocks (currentJsonContentStr : restJsonContentStrs) = do
        -- Aeson's decoders can handle leading/trailing whitespace in the input string
        -- if the core content is valid JSON.
        decodedValue <- case AE.eitherDecodeStrict' (TE.encodeUtf8 currentJsonContentStr) of
          Left err -> Left (
            "Invalid JSON in a ```json block: " <> T.pack err <>
            ". Block content (first 200 chars): " <> T.take 200 currentJsonContentStr <> "..."
            )
          Right val -> Right val

        toolCallsInBlock <- case decodedValue of
          AE.Array arrItems -> do
            let items = V.toList arrItems

                processJsonItem :: M.Map T.Text [AET.Object] -> AE.Value -> Either Text (M.Map T.Text [AET.Object])
                processJsonItem accMap item =
                  case item of
                    AE.Object obj ->
                      -- Extract tool_name (must be Text)
                      case KM.lookup "tool_name" obj of
                        Just (AE.String theToolName) ->
                          -- Extract args (can be Object or Array of one Object)
                          case KM.lookup "args" obj of
                            Just (AE.Object argsObj) -> -- Case 1: args is a JSON Object
                              Right (M.insertWith (++) theToolName [argsObj] accMap)

                            Just (AE.Array arrArgs) -> -- Case 2: args is a JSON Array
                              case V.toList arrArgs of
                                [AE.Object singleArgObj] -> -- Array must contain a single JSON Object
                                  Right (M.insertWith (++) theToolName [singleArgObj] accMap)
                                actualArrayContents -> Left (
                                  "Field 'args' for tool '" <> theToolName <>
                                  "' is an array, but it does not contain a single JSON object. " <>
                                  "Actual 'args' array content (first 200 chars): " <> T.take 200 (T.pack (show actualArrayContents)) <> "..." <>
                                  ". Full tool call item (first 200 chars): " <> T.take 200 (T.pack (show item)) <> "..."
                                  )

                            Just otherArgsValue -> Left ( -- args is neither Object nor Array, or an unhandled Array structure
                              "Field 'args' for tool '" <> theToolName <>
                              "' is not a JSON object or an array of a single JSON object. " <>
                              "Instead, 'args' was (first 200 chars): " <> T.take 200 (T.pack (show otherArgsValue)) <> "..." <>
                              ". Full tool call item (first 200 chars): " <> T.take 200 (T.pack (show item)) <> "..."
                              )
                            Nothing -> Left (
                              "Missing 'args' field for tool '" <> theToolName <> "'. Item (first 200 chars): " <>
                              T.take 200 (T.pack (show item)) <> "..."
                              )
                        Just otherToolNameValue -> Left (
                          "'tool_name' field is not a string. Found: " <> T.take 200 (T.pack (show otherToolNameValue)) <>
                          ". Item (first 200 chars): " <> T.take 200 (T.pack (show item)) <> "..."
                          )
                        Nothing -> Left (
                          "Missing 'tool_name' field in JSON object. Item (first 200 chars): " <>
                          T.take 200 (T.pack (show item)) <> "..."
                          )
                    _ -> Left (
                      "Item in JSON array is not an object: " <> T.take 200 (T.pack (show item)) <> "..."
                      )
            foldM processJsonItem M.empty items

          _ -> Left (
            "Content of ```json block is not a JSON array. Content (first 200 chars): " <>
            T.take 200 currentJsonContentStr <> "..."
            )

        -- Recursively process remaining blocks and merge results
        restToolCallsMap <- parseAndCollectBlocks restJsonContentStrs
        Right (M.unionWith (++) toolCallsInBlock restToolCallsMap)

  in if null jsonContentStrings
     then Right M.empty -- No ```json blocks found
     else parseAndCollectBlocks jsonContentStrings


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
  let
    -- 1. Attempt to extract Gemini format calls once
    geminiResults :: Either Text (M.Map T.Text [AET.Object])
    geminiResults = extractJsonToolCallsGeminiFormat txt

    -- Store any global error from the Gemini extraction itself
    geminiGlobalError :: [Text]
    geminiGlobalError = case geminiResults of
                          Left err -> [err]
                          Right _  -> []

    -- 2. For each tool, try extracting with extractFnCalls and merge with Gemini results
    attempts :: [(Tool, Either Text [AET.Object])]
    attempts =
      [ let tName = toolName tool
            -- Regular function call extraction
            fnCallAttempt :: Either Text [AET.Object]
            fnCallAttempt = extractFnCalls txt tName

            -- Objects for this tool from Gemini format (if Gemini extraction was successful)
            geminiObjsForTool :: [AET.Object]
            geminiObjsForTool = case geminiResults of
                                  Right gMap -> fromMaybe [] (M.lookup tName gMap)
                                  Left _     -> [] -- Gemini extraction failed, so no objects from it here.
                                                  -- The error is captured in geminiGlobalError.

            -- Merge: if fnCallAttempt is Right, append geminiObjsForTool.
            -- If fnCallAttempt is Left, it remains Left (error is preserved).
            combinedAttempt :: Either Text [AET.Object]
            combinedAttempt = fmap (++ geminiObjsForTool) fnCallAttempt
        in (tool, combinedAttempt)
      | tool <- tools
      ]

    -- 3. Filter out tools that had no occurrences (Right []) after merging
    relevant :: [(Tool, Either Text [AET.Object])]
    relevant =
      [ (tool, eObjs)
      | (tool, eObjs) <- attempts,
        case eObjs of
          Right [] -> False -- no occurrences (even after merge) => ignore
          _        -> True  -- either an error, or we got some objects
      ]

    -- 4. Partition into parse errors (from fnCallAttempt part) and successes
    (fnCallParseErrors, parsedOK) =
      partitionEithers
        [ case eObjs of
            Left err   -> Left err
            Right objs -> Right (tool, objs)
        | (tool, eObjs) <- relevant
        ]

    -- Combine global Gemini errors with specific function call errors
    allParseErrors :: [Text]
    allParseErrors = geminiGlobalError ++ fnCallParseErrors

  in case parsedOK of
       [] ->
         -- No tools successfully parsed (either no relevant tools or all had errors)
         if null allParseErrors
           then Right [] -- No relevant tools found, and no errors
           else Left (T.intercalate "\n---\n" allParseErrors) -- Report all accumulated errors
       _ ->
         -- At least one tool successfully parsed
         if null allParseErrors
           then -- No extraction errors at all, proceed with findErroneousToolNameCall check
             case findErroneousToolNameCall txt of
               Left specificErr -> Left specificErr -- This specific error takes precedence
               Right _          -> Right parsedOK   -- All clear, return successfully parsed tools
           else -- We have extraction errors, report them
             Left (T.intercalate "\n---\n" allParseErrors)

data TmppRet = TmppRet
  { fileFixConfirmed :: Bool,
    rationale :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON TmppRet

instance FromJSON TmppRet

findErroneousToolNameCall :: Text -> Either Text ()
findErroneousToolNameCall txt
  | T.isInfixOf "ToolName=<[" txt = mkErr "ToolName=<["
  | T.isInfixOf "ToolName<[" txt = mkErr "ToolName<["
  | otherwise = Right ()
  where
    mkErr fmt =
      Left $ "Error: found " <> fmt <> " in your response, but ToolName is not a tool! Use one of the provided tool names please."

checkRawTextPresent :: RawTexts -> AET.Object -> Either Text ()
checkRawTextPresent rawTexts obj = do
  case KM.lookup "rawTextName" obj of
    Nothing -> Right ()
    Just (AET.String valText) -> do
      case lookupText valText rawTexts of
        Just _ -> Right ()
        Nothing -> Left $ "Error: missing raw text block " <> valText
    Just oth -> Left $ "Error: rawTextName field wasn't a string, but instead: " <> show oth

checkToolArgs :: Tool -> RawTexts -> AET.Object -> Either Text ()
checkToolArgs ToolReturn _ _ = Right ()
checkToolArgs tool rawTexts obj = do
  let (argFmt, _, _) = toolArgFormatAndDesc tool
  let sampleObj = AE.decode (encodeUtf8 argFmt)
  case sampleObj of
    Nothing -> Left $ "Internal error: invalid sample json " <> argFmt
    Just sample -> do
      let matchRes = checkShapesMatch (Object sample) (Object obj)
      case matchRes of
        Left err -> Left $ "Shape of args to " <> toolName tool <> " failed to match reference " <> argFmt <> ": " <> err
        Right () -> case checkRawTextPresent rawTexts obj of
          Right () -> Right ()
          Left err -> Left $ "Error finding raw text block referenced in args for tool " <> toolName tool <> ": " <> err

processArgsOfType :: (FromJSON a) => Tool -> RawTexts -> [AET.Object] -> Either Text [a]
processArgsOfType tool rawTexts args = do
  -- First validate all args with checkToolArgs
  let validationResults = map (checkToolArgs tool rawTexts) args
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

processArgOfType :: (FromJSON a, Show a) => Tool -> RawTexts -> [AET.Object] -> Either Text a
processArgOfType tool rawTexts args = case processArgsOfType tool rawTexts args of
  Left err -> Left err
  Right vals -> case vals of
    [x] -> Right x
    oth -> Left $ "Expected single arg for " <> toolName tool <> " but got " <> show oth

processToolArgs :: (FromJSON a, Show a) => RawTexts -> Tool -> [AET.Object] -> Either Text (ToolCall a)
processToolArgs rawTexts tool@ToolOpenFile args = ToolCallOpenFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolFocusFile args = ToolCallFocusFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolCloseFile args = ToolCallCloseFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolAppendFile args = ToolCallAppendFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolReplaceFile args = ToolCallReplaceFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolEditFile args = ToolCallEditFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolEditFileByMatch args = ToolCallEditFileByMatch <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolInsertInFile args = ToolCallInsertInFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolRevertFile args = ToolCallRevertFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolSummariseAction args = ToolCallSummariseAction <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolFileLineOp args = ToolCallFileLineOp <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolAddDependency args = ToolCallAddDependency <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolPanic args = ToolCallPanic <$> processArgOfType tool rawTexts args
processToolArgs rawTexts tool@ToolEscalate args = ToolCallEscalate <$> processArgOfType tool rawTexts args
processToolArgs rawTexts tool@ToolReturn args = ToolCallReturn <$> processArgOfType tool rawTexts args

processToolsArgsNoPartial :: (FromJSON a, Show a) => [(Tool, [AET.Object])] -> RawTexts -> AppM (Either Text [ToolCall a])
processToolsArgsNoPartial toolArgs rawTexts = case partitionEithers (map (uncurry (processToolArgs rawTexts)) toolArgs) of
  ([], results) -> do
    normalisationResults <- foldAllEithers normaliseLineOpTool results
    let updErr err = "Error processing tool args: " <> err
    pure $ bimap updErr mergeToolCalls normalisationResults
  (errors, _) -> pure $ Left (T.intercalate ", " errors)

processToolsArgs ::
  (FromJSON a, Show a) =>
  [(Tool, [AET.Object])] ->
  RawTexts ->
  AppM ([Text], [ToolCall a])
processToolsArgs toolArgs rawTexts = do
  let (parseErrors, parseSuccesses) =
        partitionEithers (map (uncurry (processToolArgs rawTexts)) toolArgs)
  normalisedResults <- mapM normaliseLineOpTool parseSuccesses
  let (normaliseErrors, normaliseSuccesses) = partitionEithers normalisedResults
      allErrors =
        parseErrors
          <> map ("Error processing tool args: " <>) normaliseErrors
      merged = mergeToolCalls normaliseSuccesses
  pure (allErrors, merged)

normaliseLineOpTool :: ToolCall a -> AppM (Either Text (ToolCall a))
normaliseLineOpTool (ToolCallEditFile args) = do
  let modifyArg (EditFileArg fileName start end textBoxName) = FileLineOpArg fileName start (end + 1) textBoxName (toolName ToolEditFile)
  pure . Right . ToolCallFileLineOp $ map modifyArg args
normaliseLineOpTool (ToolCallInsertInFile args) = do
  let modifyArg (InsertInFileArg fileName lineNum textBoxName) = FileLineOpArg fileName lineNum lineNum textBoxName (toolName ToolInsertInFile)
  pure . Right . ToolCallFileLineOp $ map modifyArg args
normaliseLineOpTool (ToolCallEditFileByMatch args) = do
  st <- get
  let modifyArg :: EditFileByMatchArg -> AppM (Either Text FileLineOpArg)
      modifyArg (EditFileByMatchArg fileName startRegex startNearNum endRegex endNearNum textBoxName) =
        case getOpenFile fileName st of
          Nothing -> pure . Left $ "Cannot edit file that's not open: " <> fileName
          (Just oFile) -> do
            let contents = openFileContents oFile
                mayLineNumsInitial = getLineNumsFromRegex (startRegex, startNearNum) (endRegex, endNearNum) contents
                mayLineNums = case mayLineNumsInitial of
                  Right (startNum, endNum) -> if (abs (startNearNum - startNum) < maxLineDistanceForRegexLineMatch) && (abs (endNearNum - endNum) < maxLineDistanceForRegexLineMatch)
                    then Right (startNum, endNum)
                    else Left $ "Error: Found matching line numbers (" <> show startNum <> ", " <> show endNum <> "), but at least one of these was more than " <> show maxLineDistanceForRegexLineMatch <> " lines away from the line number it was supposed to be near, so likely a far-away line is accidentally being matched. The numbers were supposed to be near (" <> show startNearNum <> ", " <> show endNearNum <> ") respectively."
                  x -> x
                updErr err = "Error attempting to find line nums for EditFileByMatch tool: " <> err
                toLineOp (startNum, endNum) = FileLineOpArg fileName startNum (endNum + 1) textBoxName (toolName ToolEditFileByMatch)
            pure $ bimap updErr toLineOp mayLineNums
  mayNewArgs <- foldAllEithers modifyArg args
  pure $ second ToolCallFileLineOp mayNewArgs
normaliseLineOpTool x = pure $ Right x

mkSuccess :: Context -> MsgKind -> Text -> TracedEvent -> Context
mkSuccess ctxt kind txt evt = (addToContextUser ctxt kind ("Success: " <> txt)) `addEvtToContext` evt

mkErrorNoEvt :: Context -> MsgKind -> Text ->  Context
mkErrorNoEvt ctxt kind txt = addToContextUser (contextRecordError ctxt) kind ("Error: " <> txt)

mkError :: Context -> MsgKind -> Text -> TracedEvent -> Context
mkError ctxt kind txt evt = (addToContextUser (contextRecordError ctxt) kind ("Error: " <> txt)) `addEvtToContext` evt

reloadLogs :: AppM ()
reloadLogs = do
  st <- get
  updatedFiles <- traverse updateFile (stateOpenFiles st)
  put $ st {stateOpenFiles = updatedFiles}
  where
    updateFile :: OpenFile -> AppM OpenFile
    updateFile f@(OpenFile fileName _ _ _ _) =
      if ".log" `T.isSuffixOf` fileName
        then do
          cfg <- ask
          newContents <- liftIO $ FS.readFileToText (FS.toFilePath cfg fileName)
          return $ f {openFileContents = newContents}
        else return f

buildAndTest :: forall bs. (BS.BuildSystem bs) => AppM (Maybe (MsgKind, Text), [TracedEvent])
buildAndTest = do
  cfg <- ask
  let baseDir = configBaseDir cfg
  timeIONano64M (BS.buildProject @bs cfg) >>= \case
    (Just err, compileNanos) -> do
      liftIO $ Logging.logInfo "ConsiderBuildAndTest" $ "Compilation/tests failed: " <> err
      -- liftIO $ putTextLn $ "Compilation failed: " <> err
      modify' $ updateLastCompileState (Just err)
      modify' $ updateStateMetrics (mempty {metricsNumCompileFails = 1, metricsCompileTime = compileNanos})
      FS.reloadOpenFiles (getLineNumberFns @bs)
      return (Just (CompileFailMsg, err), [EvtCompileProject (FailedWithPotentiallyVeryLongError $ PotentiallyBigMessage err)])
    (Nothing, compileNanos) -> do
      liftIO $ Logging.logInfo "ConsiderBuildAndTest" "Compilation succeeded."
      let evts = [EvtCompileProject Succeeded]
      modify' $ updateLastCompileState Nothing
      (result, testNanos) <- timeIONano64M $ BS.testProject @bs cfg
      liftIO $ Logging.logInfo "ConsiderBuildAndTest" $ "Testing " <> if isJust result then "failed." else "succeeded."
      ignoredDirs <- BS.getIgnoredDirs @bs
      existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs baseDir
      modify' (updateExistingFiles existingFileNames)
      reloadLogs
      FS.reloadOpenFiles (getLineNumberFns @bs)
      modify' $ updateLastTestState result
      when (isJust result) $ modify $ updateStateMetrics (mempty {metricsNumTestFails = 1, metricsCompileTime = compileNanos, metricsTestTime = testNanos})
      {-          case result of
                  Just err -> liftIO $ putTextLn $ "Test error: " <> err
                  Nothing -> return () -}
      return $ case result of
        Just (err, numTestsFailed) -> (Just (TestFailMsg, err), evts ++ [EvtTestProject Failed numTestsFailed])
        Nothing -> (Nothing, evts ++ [EvtTestProject Succeeded $ NumFailedTests 0])

considerBuildAndTest :: forall a. (BS.BuildSystem a) => Text -> AppM (Maybe (MsgKind, Text), [TracedEvent])
considerBuildAndTest fileName = do
  isBuildable <- BS.isBuildableFile @a fileName
  case isBuildable of
    False -> return (Nothing, [])
    True -> buildAndTest @a

data RequiresOpenFile = RequiresOpenFileTrue | RequiresOpenFileFalse
  deriving (Eq, Ord, Show)

data RequiresFocusedFile = RequiresFocusedFileTrue | RequiresFocusedFileFalse
  deriving (Eq, Ord, Show)

forceFocusFile :: Text -> AppM ()
forceFocusFile fileName = do
  -- We do this so that it doesn't get immediately unfocused. TODO: think of a cleaner solution
  ts <- liftIO getCurrentPOSIXTime
  modify' $ updateFileLastModified fileName ts
  focusFile fileName
  st <- get
  let focused = fileFocused fileName st
  unless focused $ throwError $ "Failed to focus file: " <> fileName <> " :\n" <> show st
  liftIO $ Logging.logInfo "FileOperation" $ "Focusing file: " <> fileName

handleFileOperation ::
  forall bs.
  (BS.BuildSystem bs) =>
  Text ->
  (FilePath -> IO (Either Text ())) ->
  RequiresOpenFile ->
  RequiresFocusedFile ->
  Text ->
  Maybe FileChangeBounds ->
  Context ->
  AppM Context
handleFileOperation fileName ioAction requiresOpenFile requiresFocusedFile opName changeBounds ctxt = do
  theState <- get
  cfg <- ask
  liftIO $ Logging.logInfo "FileOperation" $ "Attempting action: " <> opName
  let filePath = FS.toFilePath cfg fileName
  case isFileForbidden cfg fileName of
    Just err -> do
      liftIO $ Logging.logInfo "FileOperation" $ "Attempted to modify forbidden file: " <> fileName
      pure $ mkError ctxt OtherMsg ("Error: cannot modify forbidden file. " <> err) (EvtFileOp fileName (FailedWithError "Cannot modify forbidden file"))
    Nothing -> do
      let alreadyOpen = fileAlreadyOpen fileName theState
          alreadyFocused = fileFocused fileName theState
          openOkay = alreadyOpen || requiresOpenFile == RequiresOpenFileFalse
          focusedOkay = alreadyFocused || requiresFocusedFile == RequiresFocusedFileFalse
      case (openOkay, focusedOkay) of
        (False, _) -> do
          liftIO $ Logging.logInfo "FileOperation" $ "Attempted to " <> opName <> " file that isn't open: " <> fileName
          pure $ mkError ctxt OtherMsg ("Error: cannot " <> opName <> " file that isn't open: " <> fileName) (EvtFileOp fileName (FailedWithError "Cannot modify non-opened file"))
        (_, False) -> do
          liftIO $ Logging.logInfo "FileOperation" $ "Attempted to " <> opName <> " file that isn't focused: " <> fileName
          pure $ mkError ctxt OtherMsg ("Error: cannot " <> opName <> " file that isn't focused: " <> fileName) (EvtFileOp fileName (FailedWithError "Cannot modify non-focused file"))
        (True, True) -> do
          checker <- BS.getFormatChecker @bs cfg
          let checker' :: AppM (Maybe Text)
              checker' = do
                checkRes <- liftIO checker
                case checkRes of
                  Nothing -> do
                    modify' onSyntaxCheckPass
                    pure Nothing
                  Just err -> do
                    modify' onSyntaxCheckFail
                    pure $ Just err
              op =
                if configRejectInvalidSyntaxDiffs cfg
                  then
                    FS.tryFileOp (getLineNumberFns @bs) filePath ioAction checker' changeBounds
                  else liftIO $ ioAction filePath
              onErr :: Text -> AppM Context
              onErr err = do
                liftIO $ Logging.logInfo "FileOperation" $ "File operation failed due to: " <> err
                updateFileIfExistsOnDisk @bs fileName cfg
                pure $ mkError ctxt OtherMsg err (EvtFileOp fileName (FailedWithError $ T.take 100 err))
          res <- op
          either onErr (const $ onSuccess cfg ctxt) res
  where
    onSuccess cfg ctxt' = do
      liftIO $ Logging.logInfo "FileOperation" "File operation succeeded."
      openFile @bs DoFocusOpenedFile fileName cfg
      let successMsg = "Successfully did " <> opName <> " to file " <> fileName
          successCtxt = mkSuccess ctxt' (FileModifiedMsg fileName) successMsg (EvtFileOp fileName Succeeded)
      considerBuildAndTest @bs fileName >>= \case
        (Nothing, evts) -> do
          FS.gitAddAndCommit fileName
          pure $ addEvtsToContext successCtxt evts
        (Just (msgKind, err), evts) -> pure $ (mkErrorNoEvt successCtxt msgKind (show msgKind <> ": " <> err)) `addEvtsToContext` evts

updateFileIfExistsOnDisk :: forall bs. (BS.BuildSystem bs) => Text -> Config -> AppM ()
updateFileIfExistsOnDisk fileName cfg = do
  let filePath = FS.toFilePath cfg fileName
  exists <- liftIO $ FS.fileExistsOnDisk filePath
  when exists $ openFile @bs DontFocusOpenedFile fileName cfg

data FocusOpenedFile = DoFocusOpenedFile | DontFocusOpenedFile
  deriving (Eq, Ord, Show)

getLineNumberFns :: forall bs. (BS.BuildSystem bs) => FS.LineNumberAddRemoveFns
getLineNumberFns = (BS.addLineNumberComment @bs, BS.removeLineNumberCommentIfPresent @bs)

openFile :: forall bs. (BS.BuildSystem bs) => FocusOpenedFile -> Text -> Config -> AppM ()
openFile focusOpenedFile fileName cfg = do
  st <- get
  contents <- liftIO $ FS.readFileToTextAndOpen (FS.toFilePath cfg fileName)
  isSourceFile <- BS.isBuildableFile @bs fileName
  let getContentsMinimised = do
        let fPath = FS.toFilePath cfg fileName
        actuallyExists <- liftIO $ FS.fileExistsOnDisk fPath
        if isSourceFile && actuallyExists
          then do
            minRes <- BS.minimiseFile @bs fileName
            case minRes of
              Right minimised -> pure minimised
              Left err -> liftIO $ (Logging.logInfo "OpenFile" $ "Failed to minimise file " <> fileName <> " due to error: \n " <> err) >> pure contents
          else pure contents
  contentsMinimised <- getContentsMinimised
  modify' (ensureOpenFile fileName contents contentsMinimised)
  FS.updateOpenedFile (getLineNumberFns @bs) fileName
  unless (fileExists fileName st) $ modify' (addExistingFile fileName "")
  let shouldFocus = isSourceFile && focusOpenedFile == DoFocusOpenedFile
  when shouldFocus $ forceFocusFile fileName
  liftIO $ Logging.logInfo "OpenFile" $ "Opened file " <> fileName <> ", shouldFocus: " <> show shouldFocus <> ", isSourceFile: " <> show isSourceFile

getRawText :: RawTexts -> Text -> AppM Text
getRawText rawTexts name = case lookupText name rawTexts of
  Nothing -> throwError $ "Internal error: looked up rawText named " <> name <> " but didn't find it among " <> show rawTexts
  Just txt -> pure txt

runTool :: forall bs a. (ToJSON a, FromJSON a, Show a, BS.BuildSystem bs) => RawTexts -> ToolCall a -> Context -> AppM Context
runTool _ (ToolCallOpenFile args) origCtxt = do
  theState <- get
  cfg <- ask
  let initialCtxt = origCtxt
  ctxtUpdates <- forM args $ \(OpenFileArg fileName) -> do
    case fileAlreadyOpen fileName theState of
      True -> do
        liftIO $ Logging.logInfo "FileOperation" $ "Tried to open file that's already open: " <> fileName
        pure $ \ctxt -> mkError ctxt OtherMsg ("file already open: " <> fileName) (EvtOpenFile fileName (FailedWithError "Can't open file that is already open"))
      False -> do
        openFile @bs DoFocusOpenedFile fileName cfg
        liftIO $ Logging.logInfo "FileOperation" $ "Opened file: " <> fileName
        pure $ \ctxt -> mkSuccess ctxt OtherMsg ("Opened file: " <> fileName) (EvtOpenFile fileName Succeeded)
  return $ foldl' (\acc f -> f acc) initialCtxt ctxtUpdates
runTool _ (ToolCallFocusFile args) origCtxt = do
  cfg <- ask
  let initialCtxt = origCtxt
  ctxtUpdates <- forM args $ \(FocusFileArg fileName) -> do
    theState <- get
    isSourceFile <- BS.isBuildableFile @bs fileName
    case (fileExists fileName theState, isSourceFile) of
      (False, _) -> do
        liftIO $ Logging.logInfo "FileOperation" $ "Tried to focus non-existing file: " <> fileName
        pure $ \ctxt -> mkError ctxt OtherMsg ("Cannot focus file that doesn't exist: " <> fileName) (EvtFocusFile fileName (FailedWithError "Can't focus file that doesn't exist"))
      (True, False) -> do
        liftIO $ Logging.logInfo "FileOperation" $ "Tried to focus non-source file: " <> fileName
        pure $ \ctxt -> mkError ctxt OtherMsg ("Cannot focus non-source file: " <> fileName <> ", non-source files already show all contents un-minimised") (EvtFocusFile fileName (FailedWithError "Can't focus non-source file"))
      (True, True) -> do
        unless (fileAlreadyOpen fileName theState) $ openFile @bs DoFocusOpenedFile fileName cfg
        forceFocusFile fileName
        liftIO $ Logging.logInfo "FileOperation" $ "Focused file: " <> fileName
        pure $ \ctxt -> mkSuccess ctxt OtherMsg ("Focused file: " <> fileName) (EvtFocusFile fileName Succeeded)
  return $ foldl' (\acc f -> f acc) initialCtxt ctxtUpdates
runTool _ (ToolCallCloseFile args) origCtxt = do
  let initialCtxt = origCtxt
  ctxtUpdates <- forM args $ \(CloseFileArg fileName) -> do
    theState <- get
    case fileAlreadyOpen fileName theState of
      False -> pure $ \ctxt -> mkError ctxt OtherMsg ("cannot close file that isn't open: " <> fileName) (EvtCloseFile fileName (FailedWithError "Can't close file that isn't open"))
      True -> do
        modify' (closeOpenFile fileName)
        pure $ \ctxt -> mkSuccess ctxt (FileClosedMsg fileName) ("Closed file: " <> fileName) (EvtCloseFile fileName Succeeded)
  return $ foldl' (\acc f -> f acc) initialCtxt ctxtUpdates
runTool rawTexts (ToolCallAppendFile args) origCtxt = do
  let initialCtxt = origCtxt
  ctxtUpdates <- forM args $ \(AppendFileArg fileName textName) -> pure $ \ctxt -> do
    txt <- getRawText rawTexts textName
    st <- get
    let txtNumLines = length $ T.lines txt
        fileNumLines = case getOpenFile fileName st of Just x -> length (T.lines (openFileContents x)); Nothing -> 0
        affectedBounds = Just $ FileChangeBounds (max 0 (fileNumLines - 5)) (fileNumLines + txtNumLines)
    handleFileOperation @bs
      fileName
      (`FS.appendToFile` txt)
      RequiresOpenFileFalse
      RequiresFocusedFileFalse
      "append"
      affectedBounds
      ctxt
  foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool rawTexts (ToolCallReplaceFile args) origCtxt = do
  let initialCtxt = origCtxt
  let replaceFile txt fileName = FS.clearFileOnDisk fileName >> FS.appendToFile fileName txt
  ctxtUpdates <- forM args $ \(AppendFileArg fileName textName) -> pure $ \ctxt -> do
    txt <- getRawText rawTexts textName
    handleFileOperation @bs
      fileName
      (replaceFile txt)
      RequiresOpenFileFalse
      RequiresFocusedFileTrue
      "replace"
      Nothing
      ctxt
  foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool _ (ToolCallInsertInFile args) _ = do
  throwError $ "runTool saw raw ToolCallInsertInFile " <> show args
runTool _ (ToolCallEditFile args) _ = do
  throwError $ "runTool saw raw ToolCallEditFile " <> show args
runTool _ (ToolCallEditFileByMatch args) _ = do
  throwError $ "runTool saw raw ToolCallEditFileByMatch " <> show args
runTool rawTexts (ToolCallFileLineOp args) origCtxt = do
  let initialCtxt = origCtxt
      sortedArgAttempt = validateAndSortFileLineArgs args
  case sortedArgAttempt of
    Left err -> do
      liftIO $ Logging.logInfo "FileOperation" $ "Error editing file by line arguments: " <> err
      pure $ mkError initialCtxt OtherMsg ("Error in file editing by line arguments: " <> err) (EvtFileOp "unresolved" (FailedWithError ("Edit arguments error: " <> err)))
    Right sortedArgs -> do
      ctxtUpdates <- forM sortedArgs $ \(FileLineOpArg fileName startLineNum endLineNum textName origToolName) -> pure $ \ctxt -> do
        txt <- getRawText rawTexts textName
        let affectedBounds = Just $ FileChangeBounds (max 0 (startLineNum - 5)) (endLineNum + 5)
        handleFileOperation @bs
          fileName
          (\path -> FS.replaceInFile path startLineNum endLineNum txt)
          RequiresOpenFileTrue
          RequiresFocusedFileTrue
          origToolName
          affectedBounds
          ctxt
      foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool _ (ToolCallRevertFile args) origCtxt = do
  cfg <- ask
  let initialCtxt = origCtxt
      baseDir = configBaseDir cfg
  ctxtUpdates <- forM args $ \(RevertFileArg fileName) -> pure $ \ctxt ->
    handleFileOperation @bs
      fileName
      (FS.gitRevertFile baseDir)
      RequiresOpenFileTrue
      RequiresFocusedFileTrue
      "revert"
      Nothing
      ctxt
  foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool _ (ToolCallAddDependency args) origCtxt = do
  ctxtUpdates <- forM args $ \(AddDependencyArg depName) -> pure $ \ctxt -> do
    res <- BS.addDependency @bs depName
    case res of
      Nothing -> do
        let msg = "Successfully added dependency " <> depName
        liftIO $ Logging.logInfo "AddDependency" msg
        pure $ mkSuccess ctxt OtherMsg msg (EvtAddDependency depName Succeeded)
      Just err -> do
        let errMsg = "Failed to add dependency " <> depName <> " due to error " <> err
        liftIO $ Logging.logInfo "AddDependency" errMsg
        pure $ mkError ctxt OtherMsg errMsg (EvtAddDependency depName (FailedWithError errMsg))
  foldlM (\acc f -> f acc) origCtxt ctxtUpdates
runTool _ (ToolCallSummariseAction args) ctxt = pure $ foldl (\acc evt -> addEvtToContext acc (EvtAiAction evt)) ctxt args 
runTool _ (ToolCallPanic arg) _ = throwError $ "AI panicked due to reason= " <> show arg
runTool _ (ToolCallEscalate arg) ctxt = do
  putTextLn $ "Escalation requested; please provide a solution to the following problem, terminating input with two newlines: "<> escalationReason arg
  userLines <- liftIO TerminalInput.readLinesUntilEmpty
  let userResp = T.unlines userLines
      evt = EvtEscalation (escalationReason arg) userResp
      ctxt' = (addToContextUser ctxt OtherMsg ("Supervisor provided the following response to escalation request: " <> userResp)) `addEvtToContext` evt
  liftIO $ Logging.logInfo "Escalate" (show evt)
  pure ctxt'
runTool _ (ToolCallReturn arg) ctxt = pure $ addToContextUser ctxt OtherMsg ("Attempted to return value: " <> show arg)

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

{-
main :: IO ()
main = do
  let tools = [ToolOpenFile, ToolCloseFile, ToolAppendFile, ToolEditFile, ToolReplaceFile, ToolPanic, ToolReturn]
  case findToolsCalled tmp tools of
    Left err -> putStrLn $ "Error: " <> T.unpack err
    Right results -> do
      putStrLn "Successfully parsed tools:"
      res <- processToolsArgs @MockCreatedFiles results []
      mapM_ print res
-}
