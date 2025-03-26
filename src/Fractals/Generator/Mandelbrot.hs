module Fractals.Generator.Mandelbrot
  ( generate
  ) where

import AsciiRenderer (Color(..))
import Types (TerminalSize(..))

generate :: TerminalSize -> [[Color]]
generate (TerminalSize (rows, cols)) =
  replicate rows (replicate cols (Color 1 8))  -- Temporarily all white pixels
