module Main where

import Control.Exception (bracket)
import System.Console.ANSI (hideCursor, showCursor)

main :: IO ()
main = bracket setup teardown $ \_ -> do
  putStrLn "FractalGenerator (work in progress)"

setup :: IO ()
setup = do
  hideCursor
  return ()

teardown :: () -> IO ()
teardown _ = showCursor
