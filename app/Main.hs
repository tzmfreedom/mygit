module Main where

import System.Environment
import System.IO
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
    "tree" -> treeCommand xs
    "diff" -> diffCommand xs
    "branch" -> branchCommand xs
    "checkout" -> checkoutCommand xs
    "tag" -> tagCommand xs
    "remote" -> remoteCommand xs
    "config" -> configCommand xs
    "push" -> pushCommand xs
    "server" -> serverCommand xs
    otherwise -> hPutStrLn stderr $ "no such command: " ++ x
