module Fractals.Generator
  ( FractalChoice(..)
  , generate
  ) where

import Types (TerminalSize(..))
import AsciiRenderer (Color(..))

data FractalChoice
  = MandelbrotSet
  | JuliaSet
  | SierpinskiTriangle
  deriving (Show, Eq, Enum, Bounded)

-- Dispatch: each constructor maps to its dedicated module.
-- Implementations are stubbed and will be filled in subsequent commits.
generate :: FractalChoice -> TerminalSize -> [[Color]]
generate _ (TerminalSize (rows, cols)) =
  replicate rows (replicate cols (Color 0 1))
