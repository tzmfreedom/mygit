module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  let (x:xs) = args
  case args !! 0 of
    "init" -> initCommand xs
    "add" -> addCommand xs
    "commit" -> commitCommand xs
