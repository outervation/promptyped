{-# LANGUAGE AllowAmbiguousTypes #-}

module BuildSystem where

import Core
import Relude

class BuildSystem (a :: Type) where
  buildProject :: Config -> AppM (Maybe Text)
  testProject :: Config -> AppM (Maybe Text)
  setupProject :: Config -> AppM (Maybe Text)
  isBuildableFile :: Text -> AppM Bool
  getIgnoredDirs :: AppM [Text]
  getFormatChecker :: Config -> AppM (IO (Maybe Text))
