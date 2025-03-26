module Main where

import CLI.UI (displayWelcomeScreen, displayFractalMenu, promptContinue, displayConditions)
import Fractals.Generator (generate)
import AsciiRenderer (render)
import Utils (getTerminalSize)
import Control.Exception (bracket)
import System.Console.ANSI (hideCursor, showCursor)

main :: IO ()
main = bracket setup teardown $ \_ -> do
  displayWelcomeScreen
  displayConditions
  loop
  where
    loop = do
      choice <- displayFractalMenu
      maybeSize <- getTerminalSize
      case maybeSize of
        Just size -> do
          let fractal = generate choice size
          render fractal
          continue <- promptContinue
          if continue then loop else pure ()
        Nothing -> do
          putStrLn "Error: Could not determine terminal size."
          continue <- promptContinue
          if continue then loop else pure ()

setup :: IO ()
setup = do
  hideCursor
  return ()

teardown :: () -> IO ()
teardown _ = showCursor
