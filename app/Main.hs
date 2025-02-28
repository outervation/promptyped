{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppConfig (loadConfig)
import BinanceApiDataRecorder (makeGoBinanceApiDataRecorder)
import Relude

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
      putTextLn "Usage: program <config-path> <MakeProject|RefactorProject>"
      exitFailure
