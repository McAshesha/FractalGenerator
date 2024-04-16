module Fractals.Generator
  ( FractalChoice(..)
  , generate
  , generateText
  ) where

import Types (TerminalSize(..))
import AsciiRenderer (Color(..))
import qualified Fractals.Generator.Mandelbrot as Mandelbrot
import qualified Fractals.Generator.Julia as Julia
import qualified Fractals.Generator.Sierpinski as Sierpinski

data FractalChoice
  = MandelbrotSet
  | JuliaSet
  | SierpinskiTriangle
  deriving (Show, Eq, Enum, Bounded)

generate :: FractalChoice -> TerminalSize -> [[Color]]
generate choice size = case choice of
  MandelbrotSet      -> Mandelbrot.generate size
  JuliaSet           -> Julia.generate size
  SierpinskiTriangle -> Sierpinski.generate size

-- placeholder: real SDF text rendering lands in a follow-up commit
generateText :: String -> Double -> TerminalSize -> [[Color]]
generateText _ _ (TerminalSize (r, c)) = replicate r (replicate c (Color 0 1))
