module Main where

import Test.Hspec
import Fractals.Generator.Mandelbrot

main :: IO ()
main = hspec $ do
  describe "Mandelbrot Set" $ do
    it "should escape quickly for large values" $
      mandelbrot (2 :+ 0) (MaxIterations 100) `shouldBe` Color 1 100
    it "should stay bounded for origin" $
      mandelbrot (0 :+ 0) (MaxIterations 100) `shouldBe` Color 100 100