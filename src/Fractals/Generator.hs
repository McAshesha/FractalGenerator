module Fractals.Generator
  ( FractalChoice(..)
  , generate
  ) where

import Types (TerminalSize(..))
import AsciiRenderer (Color(..))
import qualified Fractals.Generator.Mandelbrot as Mandelbrot
import qualified Fractals.Generator.Julia as Julia

data FractalChoice
  = MandelbrotSet
  | JuliaSet
  | SierpinskiTriangle
  deriving (Show, Eq, Enum, Bounded)

generate :: FractalChoice -> TerminalSize -> [[Color]]
generate choice size = case choice of
  MandelbrotSet      -> Mandelbrot.generate size
  JuliaSet           -> Julia.generate size
  SierpinskiTriangle -> let (TerminalSize (r, c)) = size in replicate r (replicate c (Color 0 1))
