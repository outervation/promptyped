{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppConfig (loadConfig)
import Relude
import TaskFromConfig qualified

main :: IO ()
main = do
  args <- getArgs
  case args of
    [appCfgPath, modelCfgPath] -> do
      cfgE <- loadConfig appCfgPath modelCfgPath
      case cfgE of
        Right (aCfg, mCfg) -> TaskFromConfig.makeTaskFromConfig aCfg mCfg
        Left err -> do
          putTextLn $ "Error loading config: " <> err
          exitFailure
    _ -> do
      putTextLn "Usage: program <config-path> <httpserver|binancedownloader>"
      exitFailure
