{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppConfig (loadConfig)
import BinanceApiDataRecorder (makeGoBinanceApiDataRecorder)
import Data.Text qualified as T
import Http11Server qualified
import Http20Server qualified
import Relude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cfgPath, kind] -> do
      cfgE <- loadConfig cfgPath
      case cfgE of
        Right cfg ->
          case kind of
            "binancedownloader" -> makeGoBinanceApiDataRecorder cfg
            "http11server" -> Http11Server.makeGoHttpServer cfg
            "http20server" -> Http20Server.makeGoHttpServer cfg
            _ -> do
              putTextLn $ "Invalid kind: " <> T.pack kind
              exitFailure
        Left err -> do
          putTextLn $ "Error loading config: " <> err
          exitFailure
    _ -> do
      putTextLn "Usage: program <config-path> <httpserver|binancedownloader>"
      exitFailure
