module Fractals.Generator.Mandelbrot
  ( generate
  , mandelbrotIter
  ) where

import AsciiRenderer (Color(..))
import Types (TerminalSize(..))
import Data.Complex (Complex(..), magnitude)
import Control.Parallel.Strategies (using, parList, rdeepseq)

generate :: TerminalSize -> [[Color]]
generate (TerminalSize (rows, cols)) =
  [ [ Color (mandelbrotIter (x col :+ y row)) 100
    | col <- [0..cols-1] ]
  | row <- [0..rows-1] ]
  `using` parList rdeepseq
  where
    x col
      | cols <= 1  = -2.5
      | otherwise  = -2.5 + (fromIntegral col / fromIntegral (cols - 1)) * 3.5
    y row
      | rows <= 1  = 1.0
      | otherwise  = 1.0 - (fromIntegral row / fromIntegral (rows - 1)) * 2.0

mandelbrotIter :: Complex Double -> Int
mandelbrotIter c = go 0 0
  where
    go z iter
      | iter >= 100 = 100
      | magnitude z > 2 = iter
      | otherwise = go (z^2 + c) (iter + 1)