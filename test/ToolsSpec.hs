{-# LANGUAGE OverloadedStrings #-}

module ToolsSpec where

import Tools (extractRawStrings)
import Relude
import Data.Text as T

import Test.Hspec

spec :: Spec
spec = describe "extractRawStrings" $ do

  it "returns an empty list if there are no raw strings" $ do
    let input = "Nothing to see here...\nJust some random text\nNo RAWSTRING markers at all."
    extractRawStrings input `shouldBe` []

  it "parses a single raw string on one line" $ do
    let input = "Here is one: RAWSTRING[simple]=[R|hello|R]. That's it."
    extractRawStrings input `shouldBe` [("simple", "hello")]

  it "parses multiple raw strings on separate lines" $ do
    let input = T.unlines
          [ "Stuff before..."
          , "RAWSTRING[first]=[R|First content|R]"
          , "In between..."
          , "RAWSTRING[second]=[R|Second content|R]"
          , "The end."
          ]
        expected =
          [ ("first",  "First content")
          , ("second", "Second content")
          ]
    extractRawStrings input `shouldBe` expected

  it "parses raw strings with multiline content" $ do
    let input = T.unlines
          [ "RAWSTRING[story]=[R|Once upon a time,"
          , "there was a line break."
          , "And another line!"
          , "|R]"
          ]
        expected =
          [ ("story", "Once upon a time,\nthere was a line break.\nAnd another line!\n")
          ]
    extractRawStrings input `shouldBe` expected

  it "parses raw string with empty content" $ do
    let input = "RAWSTRING[empty]=[R||R]"
        expected = [("empty", "")]
    extractRawStrings input `shouldBe` expected

  it "handles special characters in the name" $ do
    let input = "RAWSTRING[my-string_42!@#]=[R|some content|R]"
        expected = [("my-string_42!@#", "some content")]
    extractRawStrings input `shouldBe` expected

  it "returns multiple matches when raw strings appear back to back" $ do
    let input = "RAWSTRING[x]=[R|X|R]RAWSTRING[y]=[R|Y|R]RAWSTRING[z]=[R|Z|R]"
        expected =
          [ ("x", "X")
          , ("y", "Y")
          , ("z", "Z")
          ]
    extractRawStrings input `shouldBe` expected

  it "does not match if the syntax is incomplete (missing closing |R])" $ do
    let input = "RAWSTRING[broken]=[R|missing the end"
    extractRawStrings input `shouldBe` []

  it "does not match if the prefix doesn't say RAWSTRING" $ do
    let input = "ROWSTRING[some]=[R|this won't match|R]"
    extractRawStrings input `shouldBe` []

  it "does not produce partial matches for weird text in between" $ do
    let input = "RAWSTRING[a]=[R|content|R] but RAWSTRING[z not closed [R| oh no"
    extractRawStrings input `shouldBe` [("a","content")]

  it "can handle multiple newlines or whitespace around them" $ do
    let input = T.unlines
          [ ""
          , "   RAWSTRING[foo]=[R|foo-content|R]"
          , ""
          , "RAWSTRING[bar]=[R|bar-line1"
          , "bar-line2"
          , "|R]"
          , ""
          ]
        expected =
          [ ("foo", "foo-content")
          , ("bar", "bar-line1\nbar-line2\n")
          ]
    extractRawStrings input `shouldBe` expected
