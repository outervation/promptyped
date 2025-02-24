{-# LANGUAGE OverloadedStrings #-}

module Main where

import Relude

import BinanceApiDataRecorder (makeGoBinanceApiDataRecorder)
import AppConfig (loadConfig)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cfgPath] -> do
      cfgE <- loadConfig cfgPath
      case cfgE of
        Right cfg ->
          makeGoBinanceApiDataRecorder cfg
        Left err -> do
          putTextLn $ "Error loading config: " <> err
          exitFailure
    _ -> do
      putTextLn "Usage: program <config-path>"
      exitFailure
