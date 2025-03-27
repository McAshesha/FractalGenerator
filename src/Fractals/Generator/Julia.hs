module Fractals.Generator.Julia
  ( generate
  , juliaIter
  ) where

import AsciiRenderer (Color(..))
import Types (TerminalSize(..))
import Data.Complex (Complex(..), magnitude)
import Control.Parallel.Strategies (using, parList, rdeepseq)

generate :: TerminalSize -> [[Color]]
generate (TerminalSize (rows, cols)) =
  [ [ Color (juliaIter (x col :+ y row)) 100
    | col <- [0..cols-1] ]
  | row <- [0..rows-1] ]
  `using` parList rdeepseq
  where
    x col
      | cols <= 1  = -1.5
      | otherwise  = -1.5 + (fromIntegral col / fromIntegral (cols - 1)) * 3.0
    y row
      | rows <= 1  = 1.0
      | otherwise  = 1.0 - (fromIntegral row / fromIntegral (rows - 1)) * 2.0

juliaIter :: Complex Double -> Int
juliaIter z0 = go z0 0
  where
    c_julia = (-0.7) :+ 0.27015
    go z iter
      | iter >= 100 = 100
      | magnitude z > 2 = iter
      | otherwise = go (z^2 + c_julia) (iter + 1)