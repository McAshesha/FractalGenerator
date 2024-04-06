{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module AsciiRenderer
  ( Color(..)
  , setANSIColor
  ) where

import qualified System.Console.ANSI as ANSI
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)

data Color = Color
  { iterations :: Int
  , maxIterations :: Int
  } deriving (Eq, Show, Generic, NFData)

setANSIColor :: Int -> IO ()
setANSIColor idx = ANSI.setSGR
  [ ANSI.SetColor ANSI.Foreground
      ANSI.Dull
      (colorMapping idx)
  ]
  where
    colorMapping :: Int -> ANSI.Color
    colorMapping 0 = ANSI.Black
    colorMapping 1 = ANSI.Red
    colorMapping 2 = ANSI.Green
    colorMapping 3 = ANSI.Yellow
    colorMapping 4 = ANSI.Blue
    colorMapping 5 = ANSI.Magenta
    colorMapping 6 = ANSI.Cyan
    colorMapping 7 = ANSI.White
    colorMapping _ = ANSI.White
