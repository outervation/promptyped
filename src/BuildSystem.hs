{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module BuildSystem where

import Control.Monad.Except
import Core
import Relude

class BuildSystem (a :: Type) where
  buildProject :: Config -> AppM (Maybe Text)
  testProject :: Config -> AppM (Maybe (Text, NumFailedTests))
  setupProject :: Config -> ProjectConfig -> AppM (Maybe Text)
  isBuildableFile :: Text -> AppM Bool
  isTestFile :: Text -> AppM Bool
  getIgnoredDirs :: AppM [Text]
  getFormatChecker :: Config -> AppM (IO (Maybe Text))
  minimiseFile :: Text -> AppM (Either Text Text)
  addDependency :: Text -> AppM (Maybe Text)
  addLineNumberComment :: Int -> Text -> Text
  removeLineNumberCommentIfPresent :: Text -> Text

data NullBuildSystem = NullBuildSystem

instance BuildSystem NullBuildSystem where
  buildProject _ = throwError "BuildProject called on NullBuildSystem"
  testProject _ = throwError "TestProject called on NullBuildSystem"
  setupProject _ _ = throwError "SetupProject called on NullBuildSystem"
  isBuildableFile _ = throwError "IsBuildableFile called on NullBuildSystem"
  isTestFile _ = throwError "IsTestFile called on NullBuildSystem"
  getIgnoredDirs = throwError "getIgnoredDirs called on NullBuildSystem"
  getFormatChecker _ = throwError "getFormatChecker called on NullBuildSystem"
  minimiseFile _ = throwError "minimiseFile called on NullBuildSystem"
  addDependency _ = throwError "addDependency called on NullBuildSystem"
  addLineNumberComment _ x = x
  removeLineNumberCommentIfPresent x = x
