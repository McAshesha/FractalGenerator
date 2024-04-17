module Main where

import CLI.UI (displayWelcomeScreen, displayFractalMenu, promptContinue, displayConditions, displayResultFractal, displayErrorSizeMsg)
import Utils (getTerminalSize)
import Control.Exception (bracket)
import System.Console.ANSI (hideCursor, showCursor)

main :: IO ()
main = bracket setup teardown $ \_ -> do
  start
  loop
  where
    start = do
      maybeSize <- getTerminalSize
      case maybeSize of
        Just size -> do
          displayWelcomeScreen size
          displayConditions
        Nothing -> do
         displayErrorSizeMsg
         continue <- promptContinue
         if continue then start else pure ()

    loop = do
      maybeSize <- getTerminalSize
      case maybeSize of
        Just size -> do
          choice <- displayFractalMenu size
          displayResultFractal choice size
          continue <- promptContinue
          if continue then loop else pure ()
        Nothing -> do
          displayErrorSizeMsg
          continue <- promptContinue
          if continue then loop else pure ()

setup :: IO ()
setup = do
  hideCursor
  return ()

teardown :: () -> IO ()
teardown _ = showCursor
