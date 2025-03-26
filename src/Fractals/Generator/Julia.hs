module Fractals.Generator.Julia
  ( generate
  ) where

import AsciiRenderer (Color(..))
import Types (TerminalSize(..))

generate :: TerminalSize -> [[Color]]
generate (TerminalSize (rows, cols)) =
  [ [ Color 1 1 | _ <- [1..cols] ] | _ <- [1..rows] ]  -- Temporarily all white pixels
