module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Complex (Complex(..))
import Data.List (genericLength)
import Control.Monad (when)
import System.IO.Silently (capture) -- For capturing IO output
import qualified System.Console.ANSI as ANSI

import Types (TerminalSize(..))
import Fractals.Generator (FractalChoice(..), generate, generateText)
import Fractals.Generator.Mandelbrot (mandelbrotIter, generate)
import Fractals.Generator.Julia (juliaIter, generate)
import Fractals.Generator.Sierpinski (generate) -- Note: isSierpinski is not exported
import AsciiRenderer (Color(..), render, renderText)
import CLI.UI (displayWelcomeScreen, displayFractalMenu, promptContinue)
import Utils (getTerminalSize)

-- Custom QuickCheck generators
instance Arbitrary TerminalSize where
    arbitrary = do
        rows <- choose (1, 100) -- Reasonable terminal sizes
        cols <- choose (1, 200)
        return $ TerminalSize (rows, cols)

instance Arbitrary Color where
    arbitrary = do
        maxIter <- choose (1, 100)
        iter <- choose (0, maxIter)
        return $ Color iter maxIter

-- Helper function to compute color index (extracted from AsciiRenderer)
colorToIdx :: Color -> Int
colorToIdx color = min 7 (floor (ratio * 8))
    where ratio = fromIntegral (iterations color) / fromIntegral (maxIterations color)

-- Mock for getTerminalSize to avoid IO dependency in pure tests
mockTerminalSize :: TerminalSize
mockTerminalSize = TerminalSize (24, 80)

main :: IO ()
main = hspec $ do
    -- Types.hs Tests
    describe "Types" $ do
        describe "TerminalSize" $ do
            it "can represent valid terminal dimensions" $ do
                let size = TerminalSize (10, 20)
                getDimensions size `shouldBe` (10, 20)
            it "accepts positive dimensions" $ property $
                \(Positive rows) (Positive cols) ->
                    getDimensions (TerminalSize (rows, cols)) == (rows, cols)

    -- Fractals.Generator Tests
    describe "Fractals.Generator" $ do
        describe "generate" $ do
            it "produces a grid with correct dimensions for Mandelbrot" $ property $
                \(Positive rows) (Positive cols) ->
                    let size = TerminalSize (rows, cols)
                        grid = generate MandelbrotSet size
                    in length grid == rows && all (\row -> length row == cols) grid
            it "produces a grid with correct dimensions for Julia" $ property $
                \(Positive rows) (Positive cols) ->
                    let size = TerminalSize (rows, cols)
                        grid = generate JuliaSet size
                    in length grid == rows && all (\row -> length row == cols) grid
            it "produces a grid with correct dimensions for Sierpinski" $ property $
                \(Positive rows) (Positive cols) ->
                    let size = TerminalSize (rows, cols)
                        grid = generate SierpinskiTriangle size
                    in length grid == rows && all (\row -> length row == cols) grid
            it "handles minimal size (1x1)" $ do
                let size = TerminalSize (1, 1)
                let grid = generate MandelbrotSet size
                length grid `shouldBe` 1
                length (head grid) `shouldBe` 1

        describe "generateText" $ do
            it "produces correct dimensions for single character" $ do
                let size = TerminalSize (8, 8)
                let grid = generateText "A" 1.0 size
                length grid `shouldBe` 8 -- targetHeight based on aspect ratio
                all (\row -> length row == 8) grid `shouldBe` True
            it "maps 'A' bitmap correctly for small size" $ do
                let size = TerminalSize (8, 8)
                let grid = generateText "A" 1.0 size
                -- Check top row matches 'A' bitmap scaled
                let expectedTop = [Color 0 1, Color 0 1, Color 7 1, Color 7 1, Color 0 1, Color 0 1, Color 0 1, Color 0 1]
                (map (quantizeColor 1) (head grid)) `shouldBe` expectedTop
            it "handles empty string" $ do
                let size = TerminalSize (5, 5)
                let grid = generateText "" 1.0 size
                all (all (== Color 0 1)) grid `shouldBe` True

    -- Fractals.Generator.Mandelbrot Tests
    describe "Fractals.Generator.Mandelbrot" $ do
        describe "mandelbrotIter" $ do
            it "returns 100 for points inside the set" $ do
                mandelbrotIter (0 :+ 0) `shouldBe` 100
                mandelbrotIter (-1 :+ 0) `shouldBe` 100
                mandelbrotIter (0 :+ 1) `shouldBe` 100
            it "returns correct iteration for points outside the set" $ do
                mandelbrotIter (1 :+ 0) `shouldBe` 3
                mandelbrotIter (0.5 :+ 0) `shouldBe` 5
            it "terminates for all inputs" $ property $
                \re im -> let result = mandelbrotIter (re :+ im)
                          in result >= 0 && result <= 100

        describe "generate" $ do
            it "produces expected value at origin for 1x1" $ do
                let size = TerminalSize (1, 1)
                let grid = Mandelbrot.generate size
                head (head grid) `shouldBe` Color 100 100

    -- Fractals.Generator.Julia Tests
    describe "Fractals.Generator.Julia" $ do
        describe "juliaIter" $ do
            it "returns 1 for points with large magnitude" $ do
                juliaIter (100 :+ 0) `shouldBe` 1
                juliaIter (0 :+ 100) `shouldBe` 1
            it "terminates for all inputs" $ property $
                \re im -> let result = juliaIter (re :+ im)
                          in result >= 0 && result <= 100

    -- Fractals.Generator.Sierpinski Tests
    describe "Fractals.Generator.Sierpinski" $ do
        describe "generate" $ do
            it "produces correct grid for 2x2 size" $ do
                let size = TerminalSize (2, 2)
                let grid = Sierpinski.generate size
                grid `shouldBe` [[Color 1 1, Color 0 1], [Color 1 1, Color 1 1]]
            it "all points are either in or out of fractal" $ property $
                \(Positive rows) (Positive cols) ->
                    let size = TerminalSize (rows, cols)
                        grid = Sierpinski.generate size
                        validColor (Color i m) = (i == 1 && m == 1) || (i == 0 && m == 1)
                    in all (all validColor) grid

    -- AsciiRenderer Tests
    describe "AsciiRenderer" $ do
        describe "colorToIdx" $ do
            it "maps iterations=0 to 0" $ do
                colorToIdx (Color 0 100) `shouldBe` 0
            it "maps iterations=maxIterations to 7" $ do
                colorToIdx (Color 100 100) `shouldBe` 7
            it "maps intermediate values correctly" $ do
                colorToIdx (Color 50 100) `shouldBe` 4
                colorToIdx (Color 25 100) `shouldBe` 2
            it "produces values between 0 and 7" $ property $
                \color -> let idx = colorToIdx color
                          in idx >= 0 && idx <= 7

        describe "render" $ do
            it "renders small grid without crashing" $ do
                let grid = [[Color 50 100, Color 0 100], [Color 100 100, Color 25 100]]
                (output, ()) <- capture $ render grid
                length output `shouldSatisfy` (> 0)

        describe "renderText" $ do
            it "renders text grid with correct characters" $ do
                let grid = [[Color 0 1, Color 1 1], [Color 7 1, Color 0 1]]
                (output, ()) <- capture $ renderText grid
                output `shouldContain` " █\n█ "

    -- CLI.UI Tests
    describe "CLI.UI" $ do
        describe "displayWelcomeScreen" $ do
            it "displays welcome message" $ do
                let size = TerminalSize (24, 80)
                (output, ()) <- capture $ displayWelcomeScreen size
                output `shouldContain` "FractalGenerator"

        describe "displayFractalMenu" $ do
            it "parses valid input (mocked)" $ pendingWith "Requires IO mocking"
            -- Note: Testing IO with user input is complex; consider refactoring for purity

        describe "promptContinue" $ do
            it "handles yes/no input (mocked)" $ pendingWith "Requires IO mocking"

    -- Utils Tests
    describe "Utils" $ do
        describe "getTerminalSize" $ do
            it "returns a valid size or Nothing" $ do
                result <- getTerminalSize
                case result of
                    Just (TerminalSize (rows, cols)) -> do
                        rows `shouldSatisfy` (> 0)
                        cols `shouldSatisfy` (> 0)
                    Nothing -> return () -- Acceptable in some environments

-- Helper to quantize Color for comparison
quantizeColor :: Int -> Color -> Color
quantizeColor maxIter color = Color (colorToIdx color) maxIter