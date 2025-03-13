{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tools where

import BuildSystem qualified as BS
import Control.Monad.Except
import Core
import Data.Aeson as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AET
import Data.ByteString.Lazy qualified as LBS
import Data.List as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import FileSystem qualified as FS
import Logging qualified
import Relude
import ShapeChecker (checkShapesMatch)

data Tool = ToolOpenFile | ToolCloseFile | ToolAppendFile | ToolReplaceFile | ToolInsertInFile | ToolEditFile | ToolRevertFile | ToolFileLineOp | ToolPanic | ToolReturn
  deriving (Eq, Ord, Show)

data ToolCall a = ToolCallOpenFile [OpenFileArg] | ToolCallCloseFile [CloseFileArg] | ToolCallAppendFile [AppendFileArg] | ToolCallReplaceFile [AppendFileArg] | ToolCallEditFile [EditFileArg] | ToolCallRevertFile [RevertFileArg] | ToolCallInsertInFile [InsertInFileArg] | ToolCallFileLineOp [FileLineOpArg] | ToolCallPanic PanicArg | ToolCallReturn a
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
toolCallTool (ToolCallFileLineOp _) = ToolFileLineOp

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
        ToolCallRevertFile _ ->
          [ ToolCallRevertFile
              (concat [as | ToolCallRevertFile as <- grp])
          ]
        ToolCallFileLineOp _ ->
          [ ToolCallFileLineOp
              (concat [as | ToolCallFileLineOp as <- grp])
          ]
        -- We do not merge these, so just leave them untouched:
        ToolCallPanic _ -> grp
        ToolCallReturn _ -> grp

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

toolName :: Tool -> Text
toolName x = T.drop 4 $ show x

toolSummary :: Text
toolSummary = "You have the following tools available to you, that you may call with JSON args. Line numbers are included for your OpenFiles to simplify your task, but are not present in the files on disk (so don't explicitly write line numbers to disk!). For tools that modify files, after modification the file will be compiled if a source file, and run if it's a unit test file. \n IMPORTANT NOTE: for Append/Edit/InsertIn file, you don't provide the text as part of the json, instead you set \"rawTextName\": \"someRawTextBox\", and then afterwards include the raw string literal in C++ style RAWTEXT[someRawTextBox]=R\"r( ...the actual text... )r\". This is to avoid the need for JSON-escaping the code/text; you instead directly include the unescaped text in between the R\"r( and )r\". It allows allows multiple commands to refer to the same raw text box where necessary (e.g. if inserting the same code in multiple places). Note that LINE NUMBERS START AT ZERO, and appear at the START of the line, not the end."

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
toolArgFormatAndDesc ToolReturn = ("{ }", "", "Return a value; format depends on the task and is described further down below. Note you can only return a single value at a time!")
toolArgFormatAndDesc ToolFileLineOp = (toJ FileLineOpArg {fileName = "somefile.txt", startLineNum = 5, endLineNum = 10, rawTextName = "codeBoxToUse", origToolName = "originalToolName"}, mkSampleCodeBox "codeBoxToUse", "You should panic if you see this; it's an internal tool that insert/edit are transformed into, and you shouldn't call it directly.")
toolArgFormatAndDesc ToolOpenFile = (toJ OpenFileArg {fileName = "someFile.txt"}, "", "Load a file into the context")
toolArgFormatAndDesc ToolCloseFile = (toJ CloseFileArg {fileName = "someFile.txt"}, "", "Remove a file from the context")
toolArgFormatAndDesc ToolAppendFile = (toJ AppendFileArg {fileName = "somefile.txt", rawTextName = "codeToAppendBox"}, mkSampleCodeBox "codeToAppendBox", "Append code/text to the bottom of a file. Can be used to create a new file if the file exists.")
toolArgFormatAndDesc ToolReplaceFile = (toJ AppendFileArg {fileName = "somefile.txt", rawTextName = "codeToReplaceBox"}, mkSampleCodeBox "codeToReplaceBox", "Replace a file with the provided code/text to a file. Can be used to create a new file. Prefer this over editing when the file is small.")
toolArgFormatAndDesc ToolEditFile = (toJ EditFileArg {fileName = "somefile.txt", startLineNum = 5, endLineNum = 10, rawTextName = "codeBoxToReplaceWith"}, mkSampleCodeBox "codeBoxToReplaceWith", "Replace text in [startLineNum, endLineNum] with the text you provide. Note if making multiple edits to the same file, the start/end line numbers of different edits cannot overlap. IMPORTANT: if you insert more lines than you're replacing, the rest will be inserted, not replaced. So inserting 2 lines at at startLineNum=15 endLineNum=15 will only replace the existing line 15 in the file, and add the second provided line after that, it won't replace lines 15 and 16. Note too that the line-numbers are provided to you at the START of the line in every file. Remember line numbers start at zero.")
toolArgFormatAndDesc ToolInsertInFile = (toJ InsertInFileArg {fileName = "somefile.txt", lineNum = 17, rawTextName = "codeToInsertBox"}, mkSampleCodeBox "codeToInsertBox", "Insert the provided text into the file at lineNum, not replacing/overwriting the content on that line (instead it's moved to below the inserted text).")
toolArgFormatAndDesc ToolRevertFile = (toJ RevertFileArg {fileName = "someFile.txt"}, "", "Revert un-added changes in an open file; changes are committed when compilation and unit tests succeed, so will revert to the last version of the file before compilation or unit tests failed. Use this if you get the file in a state you can't recover it from.")
toolArgFormatAndDesc ToolPanic = (toJ PanicArg {reason = "This task is impossible for me to do because ..."}, "", "Call this if you can't complete the task due to it being impossible or not having enough information")

mkToolCallSyntax :: Tool -> Text -> Text
mkToolCallSyntax tool argFormat = toolName tool <> "=<[" <> argFormat <> "]>"

toolToDescription :: Tool -> Text
toolToDescription x = do
  let (argFormat, rawTextFormat, toolDesc) = toolArgFormatAndDesc x
  "Syntax: " <> mkToolCallSyntax x argFormat <> rawTextFormat <> "\nDescription: " <> toolDesc

returnValueToDescription :: (ToJSON a) => a -> Text
returnValueToDescription example = do
  let exampleTxt = toJ example
  let fmt = "You must return the output in a format like the following: " <> mkToolCallSyntax ToolReturn exampleTxt
  fmt <> " \n You must either return a value or call a tool. Because you're part of an automated process, you cannot prompt the user for information, so panic if you don't know how to proceed."

toolsToDescription :: [Tool] -> Text
toolsToDescription tools = toolSummary <> "\nAll available tools: \n" <> T.unlines (map toolToDescription tools) <> "\n Multiple tool calls are supported, you can either do ToolName<[{jsonArgs}]>, ToolName<[{otherJsonArgs}]>, or ToolName<[{jsonArgs}, {otherJsonArgs}]>; both are supported."

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
            Left _ -> Left "Missing opening marker '=[R|' or '=R\"r(' after name"

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
tmpp = "AppendFile=<[{\"fileName\": \"binance_ws_test.go\", \"rawTextName\": \"TestBestPriceParsingCode\"}]>\nRAWTEXT[TestBestPriceParsingCode]=[R|\nfunc TestBestPriceParsing(t *testing.T) {\n\t// Channel to receive best price events.\n\tbestPriceChan := make(chan *BestPrice, 1)\n\t// Dummy snapshot callback (not used in this test).\n\tsnapshotCb := func(symbol string) {}\n\n\t// Create a new Binance websocket client for BTCUSDT using the bookTicker (best price) stream.\n\tclient := NewBinanceWSClient(\"BTCUSDT\", \"wss://stream.binance.com:9443/ws/btcusdt@bookTicker\", snapshotCb)\n\n\t// Set the best price handler to capture best price events.\n\tclient.SetBestPriceHandler(func(bp *BestPrice) {\n\t\tselect {\n\t\tcase bestPriceChan <- bp:\n\t\tdefault:\n\t\t}\n\t})\n\n\t// Connect to the websocket.\n\tif err := client.Connect(); err != nil {\n\t\tt.Fatalf(\"Failed to connect to websocket: %v\", err)\n\t}\n\n\t// Start the websocket read loop.\n\texitCh := client.Start()\n\n\t// Wait for up to 10 seconds for a best price event.\n\tselect {\n\tcase bp := <-bestPriceChan:\n\t\t// Validate received best price event fields.\n\t\tif bp.EventTime == 0 {\n\t\t\tt.Errorf(\"Received best price event with EventTime = 0\")\n\t\t}\n\t\tif bp.Symbol == \"\" {\n\t\t\tt.Errorf(\"Received best price event with empty Symbol\")\n\t\t}\n\t\tif bp.BidPrice == \"\" {\n\t\t\tt.Errorf(\"Received best price event with empty BidPrice\")\n\t\t}\n\t\tif bp.AskPrice == \"\" {\n\t\t\tt.Errorf(\"Received best price event with empty AskPrice\")\n\t\t}\n\t\tif bp.BidQty == \"\" {\n\t\t\tt.Errorf(\"Received best price event with empty BidQty\")\n\t\t}\n\t\tif bp.AskQty == \"\" {\n\t\t\tt.Errorf(\"Received best price event with empty AskQty\")\n\t\t}\n\tcase <-time.After(10 * time.Second):\n\t\tt.Fatalf(\"Timed out waiting for a best price event\")\n\t}\n\n\t// Disconnect and wait for the read loop to exit.\n\tclient.Disconnect()\n\t<-exitCh\n}\n)r\"\n\nReturn=<[{\"unitTestPassedSuccessfully\": true}]>"

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
processToolArgs rawTexts tool@ToolCloseFile args = ToolCallCloseFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolAppendFile args = ToolCallAppendFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolReplaceFile args = ToolCallReplaceFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolEditFile args = ToolCallEditFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolInsertInFile args = ToolCallInsertInFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolRevertFile args = ToolCallRevertFile <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolFileLineOp args = ToolCallFileLineOp <$> processArgsOfType tool rawTexts args
processToolArgs rawTexts tool@ToolPanic args = ToolCallPanic <$> processArgOfType tool rawTexts args
processToolArgs rawTexts tool@ToolReturn args = ToolCallReturn <$> processArgOfType tool rawTexts args

processToolsArgs :: (FromJSON a, Show a) => [(Tool, [AET.Object])] -> RawTexts -> Either Text [ToolCall a]
processToolsArgs toolArgs rawTexts = case partitionEithers (map (uncurry (processToolArgs rawTexts)) toolArgs) of
  ([], results) -> Right . mergeToolCalls $ map normaliseLineOpTool results
  (errors, _) -> Left (T.intercalate ", " errors)

normaliseLineOpTool :: ToolCall a -> ToolCall a
normaliseLineOpTool (ToolCallEditFile args) = do
  let modifyArg (EditFileArg fileName start end textBoxName) = FileLineOpArg fileName start (end + 1) textBoxName (toolName ToolEditFile)
  ToolCallFileLineOp $ map modifyArg args
normaliseLineOpTool (ToolCallInsertInFile args) = do
  let modifyArg (InsertInFileArg fileName lineNum textBoxName) = FileLineOpArg fileName lineNum lineNum textBoxName (toolName ToolInsertInFile)
  ToolCallFileLineOp $ map modifyArg args
normaliseLineOpTool x = x

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
  let baseDir = configBaseDir cfg
  case isBuildable of
    False -> return Nothing
    True -> do
      timeIONano64M (BS.buildProject @a cfg) >>= \case
        (Just err, compileNanos) -> do
          liftIO $ Logging.logInfo "ConsiderBuildAndTest" $ "Compilation/tests failed: " <> err
          liftIO $ putTextLn $ "Compilation failed: " <> err
          modify' $ updateLastCompileState (Just err)
          modify' $ updateStateMetrics (mempty {metricsNumCompileFails = 1, metricsCompileTime = compileNanos})
          FS.reloadOpenFiles
          return $ Just (CompileFailMsg, err)
        (Nothing, compileNanos) -> do
          liftIO $ Logging.logInfo "ConsiderBuildAndTest" "Compilation and tests succeeded."
          modify' $ updateLastCompileState Nothing
          (result, testNanos) <- timeIONano64M $ BS.testProject @a cfg
          ignoredDirs <- BS.getIgnoredDirs @a
          existingFileNames <- liftIO $ FS.getFileNamesRecursive ignoredDirs baseDir
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
  liftIO $ Logging.logInfo "FileOperation" $ "Attempting action with success name: " <> successMsg
  let filePath = FS.toFilePath cfg fileName
  case isFileForbidden cfg fileName of
    Just err -> pure $ mkError ctxt OtherMsg $ "Error: cannot modify forbidden file. " <> err
    Nothing -> do
      let alreadyOpen = fileAlreadyOpen fileName theState
      if alreadyOpen || requiresOpenFile == RequiresOpenFileFalse
        then do
          checker <- BS.getFormatChecker @a cfg
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
              -- op = FS.tryFileOp filePath ioAction checker'
              op = liftIO $ ioAction filePath
              onErr :: Text -> AppM Context
              onErr err = do
                liftIO $ Logging.logInfo "FileOperation" "File operation failed."
                updateFileIfExistsOnDisk fileName cfg
                pure $ mkError ctxt OtherMsg err
          res <- op
          -- res <- liftIO $ ioAction filePath
          either onErr (const $ onSuccess cfg ctxt) res
        else pure $ mkError ctxt OtherMsg (errorPrefix <> fileName)
  where
    onSuccess cfg ctxt' = do
      liftIO $ Logging.logInfo "FileOperation" "File operation succeeded."
      openFile fileName cfg
      let successCtxt = mkSuccess ctxt' OtherMsg successMsg
      considerBuildAndTest @a fileName >>= \case
        Nothing -> do
          FS.gitAddAndCommit fileName
          pure successCtxt
        Just (msgKind, err) -> pure $ mkError successCtxt msgKind (show msgKind <> ": " <> err)

updateFileIfExistsOnDisk :: Text -> Config -> AppM ()
updateFileIfExistsOnDisk fileName cfg = do
  let filePath = FS.toFilePath cfg fileName
  exists <- liftIO $ FS.fileExistsOnDisk filePath
  when exists $ openFile fileName cfg

openFile :: Text -> Config -> AppM ()
openFile fileName cfg = do
  st <- get
  contents <- liftIO $ FS.readFileToTextAndOpen (FS.toFilePath cfg fileName)
  modify' (ensureOpenFile fileName contents)
  FS.updateOpenedFile fileName
  unless (fileExists fileName st) $ modify' (addExistingFile fileName "")
  liftIO $ Logging.logInfo "OpenFile" $ "Opened file " <> fileName <> "."

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
    let alreadyOpen = fileAlreadyOpen fileName theState
    case alreadyOpen of
      True -> pure $ \ctxt -> mkError ctxt OtherMsg ("file already open: " <> fileName)
      False -> do
        openFile fileName cfg
        pure $ \ctxt -> mkSuccess ctxt OtherMsg ("Opened file: " <> fileName)
  return $ foldl' (\acc f -> f acc) initialCtxt ctxtUpdates
runTool _ (ToolCallCloseFile args) origCtxt = do
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
runTool rawTexts (ToolCallAppendFile args) origCtxt = do
  let initialCtxt = origCtxt
  ctxtUpdates <- forM args $ \(AppendFileArg fileName textName) -> pure $ \ctxt -> do
    txt <- getRawText rawTexts textName
    handleFileOperation @bs
      fileName
      (`FS.appendToFile` txt)
      RequiresOpenFileFalse
      "cannot append to file that hasn't been opened: "
      ("Appended to file: " <> fileName)
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
      "cannot replace file that hasn't been opened: "
      ("Replaced file: " <> fileName)
      ctxt
  foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool _ (ToolCallInsertInFile args) _ = do
  throwError $ "runTool saw raw ToolCallInsertInFile " <> show args
runTool _ (ToolCallEditFile args) _ = do
  throwError $ "runTool saw raw ToolCallEditFile " <> show args
runTool rawTexts (ToolCallFileLineOp args) origCtxt = do
  let initialCtxt = origCtxt
      sortedArgAttempt = validateAndSortFileLineArgs args
  case sortedArgAttempt of
    Left err -> pure $ mkError initialCtxt OtherMsg ("Error in file editing by line arguments: " <> err)
    Right sortedArgs -> do
      ctxtUpdates <- forM sortedArgs $ \(FileLineOpArg fileName startLineNum endLineNum textName origToolName) -> pure $ \ctxt -> do
        txt <- getRawText rawTexts textName
        handleFileOperation @bs
          fileName
          (\path -> FS.replaceInFile path startLineNum endLineNum txt)
          RequiresOpenFileTrue
          ("cannot " <> origToolName <> " file that hasn't been opened: ")
          ("Did " <> origToolName <> " on file: " <> fileName)
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
      "cannot revert file that hasn't been opened: "
      ("Reverted file: " <> fileName)
      ctxt
  foldlM (\acc f -> f acc) initialCtxt ctxtUpdates
runTool _ (ToolCallPanic arg) _ = throwError $ "AI panicked due to reason= " <> show arg
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

main :: IO ()
main = do
  let tools = [ToolOpenFile, ToolCloseFile, ToolAppendFile, ToolEditFile, ToolReplaceFile, ToolPanic, ToolReturn]
  case findToolsCalled tmp tools of
    Left err -> putStrLn $ "Error: " <> T.unpack err
    Right results -> do
      putStrLn "Successfully parsed tools:"
      mapM_ print (processToolsArgs @MockCreatedFiles results [])
