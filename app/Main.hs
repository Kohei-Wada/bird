module Main where

import Game 
import Bench
import System.Environment

main :: IO ()
main = do 
    args <- getArgs 
    case args of 
      [] -> gameMain
      ["bench"] -> benchMain
      _ -> pure () 
