

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TypstSpec where

import BuildSystem
import Core
import Data.Text.IO qualified as TIO
import Relude
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Typst

main :: IO ()
main = hspec spec

-- Helper to run AppM computations, similar to other test suites in the project.
runAppMWith :: Config -> AppState -> AppM a -> IO (a, AppState)
runAppMWith config theState app = do
  res <- runApp config theState app
  case res of
    Left err -> fail $ "AppM computation failed: " ++ toString err
    Right (val, st) -> return (val, st)

spec :: Spec
spec = describe "Typst BuildSystem" $ do

  let baseConfig = mempty { configBaseDir = "/tmp" } -- dummy path
      baseState = mempty

  it "identifies .typ files as buildable" $ do
    (result, _) <- runAppMWith baseConfig baseState (isBuildableFile @Typst "document.typ")
    result `shouldBe` True
    (result2, _) <- runAppMWith baseConfig baseState (isBuildableFile @Typst "document.ty p")
    result2 `shouldBe` False
    (result3, _) <- runAppMWith baseConfig baseState (isBuildableFile @Typst "document.txt")
    result3 `shouldBe` False

  it "identifies no files as test files" $ do
    (result, _) <- runAppMWith baseConfig baseState (isTestFile @Typst "test_something.typ")
    result `shouldBe` False
    (result2, _) <- runAppMWith baseConfig baseState (isTestFile @Typst "anything.typ")
    result2 `shouldBe` False

  it "test runner always succeeds by returning Nothing" $ do
    (result, _) <- runAppMWith baseConfig baseState (testProject @Typst baseConfig)
    result `shouldBe` Nothing

  it "minimiser returns file content unchanged" $
    withSystemTempDirectory "promtyped-test" $ \tmpDir -> do
      let testFileName = "test.typ"
          testFilePath = tmpDir </> testFileName
          fileContent = "#let x = 1\n#let y = 2\n"
      TIO.writeFile testFilePath fileContent

      let config = baseConfig { configBaseDir = tmpDir }

      (result, _) <- runAppMWith config baseState (minimiseFile @Typst (toText testFileName))

      result `shouldBe` Right fileContent
