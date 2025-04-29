module TerminalInput (readLinesUntilEmpty) where

import Relude
import Data.Text qualified as T

-- Reads lines from stdin until an empty line is entered.
-- Returns the collected lines in the order they were entered.
readLinesUntilEmpty :: IO [Text]
readLinesUntilEmpty = go []
  where
    go :: [Text] -> IO [Text] -- Accumulator function
    go acc = do
      putStr "> "
      -- Ensure the prompt appears *before* waiting for input
      hFlush stdout
      -- Read a line (Relude's getLine returns IO Text)
      line <- getLine
      if T.null line -- Check if the line is empty
        then pure (reverse acc) -- Finished: return accumulated lines in correct order
        else go (line : acc)    -- Continue: add line to accumulator (in reverse order) and recurse
