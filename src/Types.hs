module Types
  ( TerminalSize(..)
  ) where

newtype TerminalSize = TerminalSize
  { getDimensions :: (Int, Int)
  } deriving (Show)
