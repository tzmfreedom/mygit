module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  let (x:xs) = args
  case x of
    "init" -> initCommand xs
    "add" -> addCommand xs
    "status" -> statusCommand xs
    "commit" -> commitCommand xs
    "log" -> logCommand xs
    "cat-file" -> catFileCommand xs
