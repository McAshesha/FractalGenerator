module Utils
  ( getTerminalSize
  ) where

import Types (TerminalSize(..))
import System.Console.Terminal.Size (size, Window(..))

getTerminalSize :: IO (Maybe TerminalSize)
getTerminalSize = do
  maybeWindow <- size
  pure $ case maybeWindow of
    Just (Window rows cols) -> Just $ TerminalSize (rows - 1, cols)
    Nothing -> Nothing
