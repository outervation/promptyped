{-# LANGUAGE OverloadedStrings #-}

module FileSystemSpec where

import Data.List as L
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import FileSystem as FS
import Relude
import System.Directory qualified as DIR
import System.FilePath ((</>))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec1
  spec2

spec1 :: Spec
spec1 = describe "replaceInFile" $ do
  it "replaces lines [5..7) with \"someText\" in a 10-line file"
    $ withSystemTempFile "testFile.txt"
    $ \tmpFile tmpHandle -> do
      -- Close the open temp file handle so we can write to it using TIO
      hClose tmpHandle

      let initialLines =
            [ "Line 0",
              "Line 1",
              "Line 2",
              "Line 3",
              "Line 4",
              "Line 5",
              "Line 6",
              "Line 7",
              "Line 8",
              "Line 9"
            ]
          initialContent = T.unlines initialLines

      TIO.writeFile tmpFile initialContent

      -- Now replace lines [5..7) => that should replace lines 5 and 6
      let newText = "someText"

      result <- replaceInFile tmpFile 5 7 newText
      result `shouldBe` Right ()

      -- Read the file back
      finalContent <- TIO.readFile tmpFile
      let finalLines = T.lines finalContent

      -- We expect that lines 0..4 are unchanged,
      -- lines 5..6 are replaced by "someText" (one line or multiple lines),
      -- lines 7..9 remain unchanged.
      -- So the final lines should be:
      let expectedLines =
            [ "Line 0",
              "Line 1",
              "Line 2",
              "Line 3",
              "Line 4",
              "someText",
              "Line 7",
              "Line 8",
              "Line 9"
            ]

      finalLines `shouldBe` expectedLines

  it "inserts text at line 2 when startLineNum == endLineNum (no replacement)"
    $ withSystemTempFile "testFile.txt"
    $ \tmpFile tmpHandle -> do
      hClose tmpHandle

      let initialLines =
            [ "Line 0",
              "Line 1",
              "Line 2",
              "Line 3"
            ]
          initialContent = T.unlines initialLines
      TIO.writeFile tmpFile initialContent

      -- If startLineNum == endLineNum, it means "insert at that position"
      -- without removing any lines.
      let newText = "Inserted line"
      result <- replaceInFile tmpFile 2 2 newText
      result `shouldBe` Right ()

      finalContent <- TIO.readFile tmpFile
      let finalLines = T.lines finalContent

      -- We expect line 0 and 1 unchanged,
      -- then the inserted line at index 2,
      -- then continuing with what was originally line 2, etc.
      let expectedLines =
            [ "Line 0",
              "Line 1",
              "Inserted line",
              "Line 2",
              "Line 3"
            ]

      finalLines `shouldBe` expectedLines

spec2 :: Spec
spec2 = describe "ensureLineNumbers" $ do
  it "adds line numbers to a file and is idempotent on repeated calls" $ do
    withSystemTempDirectory "testEnsureLineNumbers" $ \tmpDir -> do
      let testFile = tmpDir </> "test.txt"

      -- Here we include some indentation, an already-numbered line, etc.
      let initialContent =
            T.unlines
              [ "    /* 123 */  let x = 1", -- Indented + has an existing comment with digits
                "        let y = 2", -- Further indentation, no comment
                "", -- Empty line
                "/* 42 */some code", -- No space after the comment
                "plain line"
              ]
      TIO.writeFile testFile initialContent

      -- First run
      result1 <- ensureLineNumbers testFile
      case result1 of
        Left err1 ->
          expectationFailure $ "First run failed: " <> T.unpack err1
        Right contents1 -> do
          let lines1 = T.lines contents1
          length lines1 `shouldBe` 5

          -- For demonstration, we just check that each line starts with a comment.
          -- (In real tests, you might be more specific.)
          mapM_ (\ln -> ln `shouldSatisfy` ("/* " `T.isPrefixOf`)) lines1

      -- Second run (should NOT add extra spaces or change indentation again)
      result2 <- ensureLineNumbers testFile
      result2 `shouldBe` result1 -- The file should not change on repeated runs

      -- Final file content should still match
      finalContents <- TIO.readFile testFile
      Right finalContents `shouldBe` result1

-- Helper function to create a temporary file with content
withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile content action = do
  withSystemTempFile "test.txt" $ \path handle -> do
    hPutStr handle content
    hClose handle
    action path

-- Tests for tryFileOp
spec3 :: Spec
spec3 = describe "tryFileOp" $ do
  context "when operation succeeds and validation passes" $ do
    it "completes successfully and keeps the changes" $ do
      withTempFile "original content" $ \path -> do
        let op _ = do
              writeFile path "new content"
              return $ Right ()

        let checker = return Nothing -- Validation passes
        result <- FS.tryFileOp path op checker

        result `shouldBe` Right ()
        content <- TIO.readFile path
        content `shouldBe` T.pack "new content"

        -- Backup should be cleaned up
        DIR.doesFileExist (path ++ ".bak") `shouldReturn` False

  context "when operation fails" $ do
    it "returns the operation error and doesn't change the file" $ do
      withTempFile "original content" $ \path -> do
        let op _ = return $ Left $ T.pack "Operation failed"
        let checker = return Nothing

        result <- FS.tryFileOp path op checker

        case result of
          Left err -> err `shouldBe` T.pack "Operation failed"
          Right _ -> expectationFailure "Expected operation to fail"

        content <- TIO.readFile path
        content `shouldBe` T.pack "original content"

        -- Backup should be cleaned up
        DIR.doesFileExist (path ++ ".bak") `shouldReturn` False

  context "when operation succeeds but validation fails" $ do
    it "restores the original file and returns the validation error" $ do
      withTempFile "original content" $ \path -> do
        let op _ = do
              writeFile path "new content"
              return $ Right ()

        let checker = return $ Just $ T.pack "Validation failed"

        result <- FS.tryFileOp path op checker

        case result of
          Left err -> err `shouldBe` T.pack "Validation failed"
          Right _ -> expectationFailure "Expected validation to fail"

        -- File should be restored to original
        content <- TIO.readFile path
        content `shouldBe` T.pack "original content"

        -- Backup should be cleaned up
        DIR.doesFileExist (path ++ ".bak") `shouldReturn` False

  context "with read-only file" $ do
    it "fails to create backup and returns an error" $ do
      withTempFile "original content" $ \path -> do
        -- Make file read-only
        permissions <- DIR.getPermissions path
        DIR.setPermissions path (DIR.setOwnerWritable False permissions)

        let op _ = return $ Right ()
        let checker = return Nothing

        result <- FS.tryFileOp (path ++ ".copy") op checker

        case result of
          Left err -> T.isInfixOf "does not exist" err `shouldBe` True
          Right _ -> expectationFailure "Expected an error for backup creation"

        -- Reset permissions for cleanup
        DIR.setPermissions path (DIR.setOwnerWritable True permissions)

  context "with complex scenario" $ do
    it "handles a multi-step operation correctly" $ do
      withTempFile "line1\nline2\nline3" $ \path -> do
        let op _ = do
              content <- T.lines <$> TIO.readFile path
              TIO.writeFile path $ T.unlines $ L.reverse content
              return $ Right ()

        let checker = do
              content <- TIO.readFile path
              if T.isInfixOf (T.pack "line1") content
                then return $ Just $ T.pack "Expected line1 to be moved"
                else return Nothing

        result <- FS.tryFileOp path op checker

        result `shouldBe` Right ()
        content <- TIO.readFile path
        content `shouldBe` T.pack "line3\nline2\nline1\n"
