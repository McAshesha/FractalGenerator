module Fractals.Generator.Sierpinski
  ( generate
  ) where

import AsciiRenderer (Color(..))
import Types (TerminalSize(..))

generate :: TerminalSize -> [[Color]]
generate (TerminalSize (rows, cols)) =
  take rows $ repeat (take cols $ repeat (Color 1 1))  -- Temporarily all white pixels
