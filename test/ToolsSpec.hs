{-# LANGUAGE OverloadedStrings #-}

module ToolsSpec where

import Data.Text as T
import Relude
import Test.Hspec
import Tools

type ToolCallUnit = ToolCall Text

spec :: Spec
spec = describe "extractRawStrings" $ do
  it "returns an empty list if there are no raw strings" $ do
    let input = "Nothing to see here...\nJust some random text\nNo RAWTEXT markers at all."
    extractRawStrings input `shouldBe` (Right [])

  it "parses a single raw string on one line" $ do
    let input = "Here is one: RAWTEXT[simple]=R\"r(hello)r\". That's it."
    extractRawStrings input `shouldBe` (Right [("simple", "hello")])

  it "parses multiple raw strings on separate lines" $ do
    let input =
          T.unlines
            [ "Stuff before...",
              "RAWTEXT[first]=R\"r(First content)r\"",
              "In between...",
              "RAWTEXT[second]=R\"r(Second content)r\"",
              "The end."
            ]
        expected =
          Right
            [ ("first", "First content"),
              ("second", "Second content")
            ]
    extractRawStrings input `shouldBe` expected

  it "parses raw strings with multiline content" $ do
    let input =
          T.unlines
            [ "RAWTEXT[story]=R\"r(Once upon a time,",
              "there was a line break.",
              "And another line!",
              ")r\""
            ]
        expected =
          Right
            [ ("story", "Once upon a time,\nthere was a line break.\nAnd another line!\n")
            ]
    extractRawStrings input `shouldBe` expected

  it "parses raw string with empty content" $ do
    let input = "RAWTEXT[empty]=R\"r()r\""
        expected = Right [("empty", "")]
    extractRawStrings input `shouldBe` expected

  it "handles special characters in the name" $ do
    let input = "RAWTEXT[my-string_42!@#]=R\"r(some content)r\""
        expected = Right [("my-string_42!@#", "some content")]
    extractRawStrings input `shouldBe` expected

  it "returns multiple matches when raw strings appear back to back" $ do
    let input = "RAWTEXT[x]=R\"r(X)r\"RAWTEXT[y]=R\"r(Y)r\"RAWTEXT[z]=R\"r(Z)r\""
        expected =
          Right
            [ ("x", "X"),
              ("y", "Y"),
              ("z", "Z")
            ]
    extractRawStrings input `shouldBe` expected

  it "does not match if the syntax is incomplete (missing closing )r\")" $ do
    let input = "RAWTEXT[broken]=R\"r(missing the end"
    extractRawStrings input `shouldBe` (Left "Could not find matching close marker )r\" for broken")

  it "does not match if the prefix doesn't say RAWTEXT" $ do
    let input = "ROWSTRING[some]=R\"r(this won't match)r\""
    extractRawStrings input `shouldBe` (Right [])

  it "does not produce partial matches for weird text in between" $ do
    let input = "RAWTEXT[a]=R\"r(content)r\" but RAWTEXT[z not closed R\"r( oh no"
    extractRawStrings input `shouldBe` (Left "Missing ']' after RAWTEXT name")

  it "can handle multiple newlines or whitespace around them" $ do
    let input =
          T.unlines
            [ "",
              "   RAWTEXT[foo]=R\"r(foo-content)r\"",
              "",
              "RAWTEXT[bar]=R\"r(bar-line1",
              "bar-line2",
              ")r\"",
              ""
            ]
        expected =
          Right
            [ ("foo", "foo-content"),
              ("bar", "bar-line1\nbar-line2\n")
            ]
    extractRawStrings input `shouldBe` expected

  it "handles a string with unicode in it" $ do
    let input = "Below are two tool calls that will update the websocket client to (a) no longer log errors when a message lacks an \"e\" field (instead logging an info and returning) and (b) set a pong handler in the Connect function to help keep the connection alive.\n\nFirst, edit processMessage in binance_ws.go (lines 125\8211\&129):\n\nEditFile=<[{\"fileName\": \"binance_ws.go\", \"startLineNum\": 125, \"endLineNum\": 129, \"rawTextName\": \"processMessageReplacement\"}]>\nRAWTEXT[processMessageReplacement]=R\"r(\n\teventType, ok := msgMap[\"e\"].(string)\n\tif !ok {\n\t\t// Ignore messages that lack an event type (e.g., pings or control frames)\n\t\tLogInfo(\"Ignoring message without event type field\")\n\t\treturn\n\t}\n)r\"\n\nNext, edit the connection setup in Connect (replace lines 55\8211\&57):\n\nEditFile=<[{\"fileName\": \"binance_ws.go\", \"startLineNum\": 55, \"endLineNum\": 57, \"rawTextName\": \"connectReplacement\"}]>\nRAWTEXT[connectReplacement]=R\"r(\n\tc.conn = conn\n\tc.conn.SetPongHandler(func(appData string) error {\n\t\t// Extend the read deadline upon receiving a pong.\n\t\tc.conn.SetReadDeadline(time.Now().Add(60 * time.Second))\n\t\treturn nil\n\t})\n\tLogInfo(fmt.Sprintf(\"Connected to websocket for %s\", c.Instrument))\n\treturn nil\n)r\"\n\nThese changes should prevent our client from treating ping/control messages as errors and may help prevent the \"use of closed network connection\" errors by properly handling pong responses."
        expected =
          Right
            [ ("processMessageReplacement", "\n\teventType, ok := msgMap[\"e\"].(string)\n\tif !ok {\n\t\t// Ignore messages that lack an event type (e.g., pings or control frames)\n\t\tLogInfo(\"Ignoring message without event type field\")\n\t\treturn\n\t}\n"),
              ("connectReplacement", "\n\tc.conn = conn\n\tc.conn.SetPongHandler(func(appData string) error {\n\t\t// Extend the read deadline upon receiving a pong.\n\t\tc.conn.SetReadDeadline(time.Now().Add(60 * time.Second))\n\t\treturn nil\n\t})\n\tLogInfo(fmt.Sprintf(\"Connected to websocket for %s\", c.Instrument))\n\treturn nil\n")
            ]
    extractRawStrings input `shouldBe` expected

  describe "mergeToolCalls" $ do
    it "returns an empty list when given an empty list" $ do
      (mergeToolCalls ([] :: [ToolCallUnit])) `shouldBe` ([] :: [ToolCallUnit])

    it "leaves a single call untouched" $ do
      let single :: ToolCallUnit
          single = ToolCallOpenFile [OpenFileArg "fileA"]
      mergeToolCalls [single] `shouldBe` [single]

    it "merges consecutive ToolCallOpenFile calls" $ do
      let call1, call2 :: ToolCallUnit
          call1 = ToolCallOpenFile [OpenFileArg "fileA"]
          call2 = ToolCallOpenFile [OpenFileArg "fileB"]
      mergeToolCalls [call1, call2]
        `shouldBe` [ToolCallOpenFile [OpenFileArg "fileA", OpenFileArg "fileB"]]

    it "does not merge non-consecutive calls of the same type" $ do
      let open1, open2 :: ToolCallUnit
          open1 = ToolCallOpenFile [OpenFileArg "file1"]
          open2 = ToolCallOpenFile [OpenFileArg "file3"]

          close1 :: ToolCallUnit
          close1 = ToolCallCloseFile [CloseFileArg "file2"]

      mergeToolCalls [open1, close1, open2]
        `shouldBe` [open1, close1, open2]

    it "merges consecutive calls of different types separately" $ do
      let close1, close2 :: ToolCallUnit
          close1 = ToolCallCloseFile [CloseFileArg "foo"]
          close2 = ToolCallCloseFile [CloseFileArg "bar"]

          append1, append2 :: ToolCallUnit
          append1 = ToolCallAppendFile [AppendFileArg "fileX" "txtA"]
          append2 = ToolCallAppendFile [AppendFileArg "fileY" "txtB"]

      mergeToolCalls [close1, close2, append1, append2]
        `shouldBe` [ ToolCallCloseFile
                       [ CloseFileArg "foo",
                         CloseFileArg "bar"
                       ],
                     ToolCallAppendFile
                       [ AppendFileArg "fileX" "txtA",
                         AppendFileArg "fileY" "txtB"
                       ]
                   ]

    it "leaves Panic calls as-is (not merged) even if consecutive" $ do
      let panic1, panic2 :: ToolCallUnit
          panic1 = ToolCallPanic $ PanicArg "oh no"
          panic2 = ToolCallPanic $ PanicArg "something else"
      mergeToolCalls [panic1, panic2] `shouldBe` [panic1, panic2]

    it "leaves Return calls as-is (not merged) even if consecutive" $ do
      -- Here we need a specific type for the return, e.g. Text
      let ret1 = ToolCallReturn @Text "return1"
          ret2 = ToolCallReturn @Text "return2"
      mergeToolCalls ([ret1, ret2] :: [ToolCall Text]) `shouldBe` [ret1, ret2]

    it "merges calls around a Panic/Return without crossing them" $ do
      let openA, openB, openC, openD :: ToolCallUnit
          openA = ToolCallOpenFile [OpenFileArg "fileA"]
          openB = ToolCallOpenFile [OpenFileArg "fileB"]
          openC = ToolCallOpenFile [OpenFileArg "fileC"]
          openD = ToolCallOpenFile [OpenFileArg "fileD"]

          panic1 :: ToolCallUnit
          panic1 = ToolCallPanic $ PanicArg "Stop!"

          ret1 :: ToolCall Text
          ret1 = ToolCallReturn "done"

      -- openA & openB should merge into one,
      -- then panic is separate,
      -- then openC is separate,
      -- then return is separate,
      -- then openD is separate
      mergeToolCalls
        ( [openA, openB, panic1, openC, ret1, openD] ::
            [ToolCall Text]
        )
        `shouldBe` [ ToolCallOpenFile [OpenFileArg "fileA", OpenFileArg "fileB"],
                     ToolCallPanic $ PanicArg "Stop!",
                     ToolCallOpenFile [OpenFileArg "fileC"],
                     ToolCallReturn "done",
                     ToolCallOpenFile [OpenFileArg "fileD"]
                   ]

    it "merges consecutive EditFile calls" $ do
      let edit1, edit2 :: ToolCallUnit
          edit1 =
            ToolCallEditFile
              [ EditFileArg
                  { fileName = "myFile",
                    startLineNum = 1,
                    endLineNum = 2,
                    rawTextName = "some text"
                  }
              ]
          edit2 =
            ToolCallEditFile
              [ EditFileArg
                  { fileName = "myFile",
                    startLineNum = 3,
                    endLineNum = 4,
                    rawTextName = "more text"
                  }
              ]
      (mergeToolCalls [edit1, edit2] :: [ToolCallUnit])
        `shouldBe` [ ToolCallEditFile
                       [ EditFileArg "myFile" 1 2 "some text",
                         EditFileArg "myFile" 3 4 "more text"
                       ]
                   ]

    it "merges consecutive FileLineOp calls" $ do
      let op1, op2 :: ToolCallUnit
          op1 =
            ToolCallFileLineOp
              [ FileLineOpArg
                  { fileName = "myFile",
                    startLineNum = 10,
                    endLineNum = 10,
                    rawTextName = "replace line",
                    origToolName = "someTool"
                  }
              ]
          op2 =
            ToolCallFileLineOp
              [ FileLineOpArg
                  { fileName = "myFile",
                    startLineNum = 11,
                    endLineNum = 12,
                    rawTextName = "replace lines",
                    origToolName = "someTool"
                  }
              ]
      (mergeToolCalls [op1, op2] :: [ToolCallUnit])
        `shouldBe` [ ToolCallFileLineOp
                       [ FileLineOpArg "myFile" 10 10 "replace line" "someTool",
                         FileLineOpArg "myFile" 11 12 "replace lines" "someTool"
                       ]
                   ]
