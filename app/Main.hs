{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppConfig (loadConfig)
import BinanceApiDataRecorder (makeGoBinanceApiDataRecorder)
import Data.Text qualified as T
import Http11Server qualified
import Http20Server qualified
import Relude
import TaskFromConfig qualified

main :: IO ()
main = do
  args <- getArgs
  case args of
    [appCfgPath, modelCfgPath, kind] -> do
      cfgE <- loadConfig appCfgPath modelCfgPath
      case cfgE of
        Right (aCfg, mCfg) ->
          case kind of
            "binancedownloader" -> makeGoBinanceApiDataRecorder aCfg mCfg
            "http11server" -> Http11Server.makeGoHttpServer aCfg mCfg
            "http20server" -> Http20Server.makeGoHttpServer aCfg mCfg
            "taskFromConfig" -> TaskFromConfig.makeTaskFromConfig aCfg mCfg
            _ -> do
              putTextLn $ "Invalid kind: " <> T.pack kind
              exitFailure
        Left err -> do
          putTextLn $ "Error loading config: " <> err
          exitFailure
    _ -> do
      putTextLn "Usage: program <config-path> <httpserver|binancedownloader>"
      exitFailure
