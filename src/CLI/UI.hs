module CLI.UI
  ( displayWelcomeScreen
  , displayFractalMenu
  , renderScreen
  ) where

import qualified System.Console.ANSI as ANSI
import Fractals.Generator (FractalChoice(..))

displayWelcomeScreen :: IO ()
displayWelcomeScreen = do
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
  putStrLn "\nPlease maximize your terminal window."
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

renderScreen :: [String] -> IO ()
renderScreen rows = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  mapM_ putStrLn rows
  _ <- getChar
  return ()
