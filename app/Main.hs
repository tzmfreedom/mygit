module Main where

import Lib

main :: IO ()
main = do
  args <- getArgs
  let (x:xs) = args
  case args !! 0 of
    "add" -> addCommand xs
    "commit" -> commitCommand xs
