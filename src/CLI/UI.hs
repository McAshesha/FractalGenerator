module CLI.UI
  ( displayWelcomeScreen
  , displayFractalMenu
  , displayResultFractal
  , displayErrorSizeMsg
  , renderScreen
  , promptContinue
  , displayConditions
  ) where

import qualified System.Console.ANSI as ANSI
import Fractals.Generator (FractalChoice(..), generate, generateText)
import Data.Char (toUpper)
import AsciiRenderer (render, renderText)
import Types (TerminalSize(..))

displayResultFractal :: FractalChoice -> TerminalSize -> IO ()
displayResultFractal choice size = do
  let fractal = generate choice size
  render fractal

displayErrorSizeMsg :: IO ()
displayErrorSizeMsg = putStrLn "Error: Could not determine terminal size."

displayWelcomeLabel :: TerminalSize -> IO ()
displayWelcomeLabel (TerminalSize (rows, cols)) = if rows <= 1200 && cols <= 300
    then do
      ANSI.clearScreen
      ANSI.setCursorPosition 0 0
      ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
      putStrLn "_____________________    _____  _________________________  .____     "
      putStrLn "\\_   _____/\\______   \\  /  _  \\ \\_   ___ \\__    ___/  _  \\ |    |    "
      putStrLn " |    __)   |       _/ /  /_\\  \\/    \\  \\/ |    | /  /_\\  \\|    |    "
      putStrLn " |     \\    |    |   \\/    |    \\     \\____|    |/    |    \\    |___ "
      putStrLn " \\___  /    |____|_  /\\____|__  /\\______  /|____|\\____|__  /_______ \\"
      putStrLn "     \\/            \\/         \\/        \\/               \\/        \\/"
      putStrLn "  ___________________ _______  _____________________    ________________________ __________ "
      putStrLn " /  _____/\\_   _____/ \\      \\ \\_   _____/\\______   \\  /  _  \\__    ___/\\_____  \\\\______   \\"
      putStrLn "/   \\  ___ |    __)_  /   |   \\ |    __)_  |       _/ /  /_\\  \\|    |    /   |   \\|       _/"
      putStrLn "\\    \\_\\  \\|        \\/    |    \\|        \\ |    |   \\/    |    \\    |   /    |    \\    |   \\"
      putStrLn " \\______  /_______  /\\____|__  /_______  / |____|_  /\\____|__  /____|   \\_______  /____|_  /"
      putStrLn "        \\/        \\/         \\/        \\/         \\/         \\/                 \\/       \\/ "
      ANSI.setSGR [ANSI.Reset]
    else do
      let labelFractal = generateText "FRACTAL" 0.6 (TerminalSize (rows, cols))
      let labelGENERATOR = generateText "GENERATOR" 0.6 (TerminalSize (rows, cols))
      ANSI.clearScreen
      ANSI.setCursorPosition 0 0
      ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
      renderText labelFractal
      renderText labelGENERATOR


displayWelcomeScreen :: TerminalSize -> IO ()
displayWelcomeScreen size = do
  displayWelcomeLabel size
  putStrLn "\nFractalGenerator is a simple mathematical fractal generator"
  putStrLn "that allows you to explore the beauty of mathematical fractals"
  putStrLn "in a basic console mode. You will be presented with a selection of fractals,"
  putStrLn "and you will need to choose the one that interests you."
  putStrLn "Press <Enter> to continue..."
  _ <- getLine
  ANSI.clearScreen

displayConditions :: IO ()
displayConditions = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  putStrLn "For a more comfortable viewing experience of mathematical fractals,"
  putStrLn "you should maximize the terminal window and select the smallest possible font size."
  putStrLn "This will allow the fractal to be displayed in the terminal with the highest clarity."
  putStrLn "Press <Enter> to continue..."
  _ <- getLine
  ANSI.clearScreen

data MenuOption = Mandelbrot | Julia | Sierpinski deriving (Show, Enum, Bounded)

displayFractalMenu :: TerminalSize -> IO FractalChoice
displayFractalMenu size = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  putStrLn "Choose a fractal:\n"
  putStrLn "1) Mandelbrot Set"
  putStrLn "2) Julia Set"
  putStrLn "3) Sierpinski Triangle\n"
  putStr "Enter your choice (1-3): "
  choice <- getLine
  case reads choice of
    [(n, _)] | n >= 1 && n <= 3 -> return $ toEnum (n-1)
    _ -> displayFractalMenu size

promptContinue :: IO Bool
promptContinue = do
  putStrLn "Do you want to generate another fractal? Y(es)/N(o): "
  input <- getLine
  case map toUpper input of
    "Y" -> pure True
    "N" -> do
      ANSI.clearScreen
      ANSI.setCursorPosition 0 0
      return False
    _   -> do
      ANSI.clearScreen
      ANSI.setCursorPosition 0 0
      putStrLn "Invalid input. Please enter Y or N."
      promptContinue

renderScreen :: [String] -> IO ()
renderScreen rows = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  mapM_ putStrLn rows
  _ <- getChar
  return ()
