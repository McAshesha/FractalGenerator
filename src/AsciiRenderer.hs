{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module AsciiRenderer
  ( render
  , Color(..)
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

    -- Map our 0-7 index to ANSI colors
    colorMapping :: Int -> ANSI.Color
    colorMapping 0 = ANSI.Black
    colorMapping 1 = ANSI.Red
    colorMapping 2 = ANSI.Green
    colorMapping 3 = ANSI.Yellow
    colorMapping 4 = ANSI.Blue
    colorMapping 5 = ANSI.Magenta
    colorMapping 6 = ANSI.Cyan
    colorMapping 7 = ANSI.White
    colorMapping _ = ANSI.White  -- Default case

-- Render a 2D grid of colors to the terminal
render :: [[Color]] -> IO ()
render colors = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  mapM_ renderRow colors
  ANSI.setSGR [ANSI.Reset]
  _ <- getLine  -- Wait for Enter to leave fractal view
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  where
    renderRow row = do
      mapM_ renderPixel row
      putStrLn ""

    renderPixel color = do
      let ratio = fromIntegral (iterations color) / fromIntegral (maxIterations color)
      let colorIdx = min 7 (floor (ratio * 8))  -- Ensure index is within 0-7
      setANSIColor colorIdx
      putStr "█"
