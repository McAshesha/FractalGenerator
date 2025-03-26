module Fractals.Generator.Sierpinski
  ( generate
  ) where

import AsciiRenderer (Color(..))
import Types (TerminalSize(..))
import Data.Bits ((.&.), shiftR)
import Control.Parallel.Strategies (using, parList, rdeepseq)
import Control.DeepSeq (NFData(..))

generate :: TerminalSize -> [[Color]]
generate (TerminalSize (rows, cols)) =
  [ [ if isSierpinski (x col) (y row)
      then Color 1 1  -- Part of the fractal
      else Color 0 1  -- Background
    | col <- [0..cols-1] ]
  | row <- [0..rows-1] ]
  `using` parList rdeepseq
  where
    -- Map terminal coordinates to [0, 1] range
    x col = fromIntegral col / fromIntegral (cols - 1)
    y row = fromIntegral (rows - 1 - row) / fromIntegral (rows - 1)

    -- Sierpinski check with 10 iterations of precision
    isSierpinski :: Double -> Double -> Bool
    isSierpinski x y = go (truncate (x * 2^10) :: Integer) (truncate (y * 2^10) :: Integer)
      where
        go :: Integer -> Integer -> Bool
        go x' y'
          | x' == 0 || y' == 0 = True
          | x' .&. y' /= 0     = False  -- Collision in binary digits
          | otherwise          = go (x' `shiftR` 1) (y' `shiftR` 1)