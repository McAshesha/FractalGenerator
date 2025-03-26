module Main where

import CLI.UI (displayWelcomeScreen, displayFractalMenu)
import Fractals.Generator (generate)
import AsciiRenderer (render)
import Utils (getTerminalSize)
import Control.Exception (bracket)
import System.Console.ANSI (hideCursor, showCursor)

main :: IO ()
main = bracket setup teardown $ \_ -> do
  displayWelcomeScreen
  choice <- displayFractalMenu
  maybeSize <- getTerminalSize
  case maybeSize of
    Just size -> do
      let fractal = generate choice size
      render fractal
      _ <- getLine
      return ()
    Nothing -> putStrLn "Error: Could not determine terminal size."

setup :: IO ()
setup = do
  hideCursor
  return ()

teardown :: () -> IO ()
teardown _ = showCursor
