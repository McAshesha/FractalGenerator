module CLI.UI
  ( displayWelcomeScreen
  , displayFractalMenu
  , renderScreen
  , promptContinue
  , displayConditions
  ) where

import qualified System.Console.ANSI as ANSI
import Fractals.Generator (FractalChoice(..))
import Data.Char (toUpper)

displayWelcomeScreen :: IO ()
displayWelcomeScreen = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  putStrLn "  ______           _        _   _       _             "
  putStrLn " |  ____|         | |      | | | |     | |            "
  putStrLn " | |__ _ __ _ __ | |_ __ _| | | | __ _| | _____ _ __ "
  putStrLn " |  __| '__| '_ \\| __/ _` | | | |/ _` | |/ / _ \\ '__|"
  putStrLn " | |  | |  | |_) | || (_| | |_| | (_| |   <  __/ |   "
  putStrLn " |_|  |_|  | .__/ \\__\\__,_|\\___/ \\__,_|_|\\_\\___|_|   "
  putStrLn "            | |                                      "
  putStrLn "            |_|                                      "
  ANSI.setSGR [ANSI.Reset]
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

displayFractalMenu :: IO FractalChoice
displayFractalMenu = do
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
    _ -> displayFractalMenu

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
