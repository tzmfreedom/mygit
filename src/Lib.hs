{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( initCommand,
      addCommand,
      commitCommand,
      logsCommand,
      statusCommand,
      catFileCommand,
    ) where

import Data.ByteString as B
import Data.Hex
import Data.List as L
import Data.List.Split
import System.Directory
import Control.Monad.Extra
import Codec.Binary.UTF8.String
import qualified Crypto.Hash.SHA1 as SHA1

data Object = Object{
  objectPerm :: [Int],
  objectType :: String,
  objectHash :: String,
  objectName :: String
} deriving Show

myGitDirectory :: String
myGitDirectory = ".mygit"

refsDirectory :: String
refsDirectory = "refs"

objectDirectory :: String
objectDirectory = "objects"

indexFile :: String
indexFile = "index"

initCommand :: [String] -> IO ()
initCommand args = do
  ifM (doesFileExist myGitDirectory) (removeDirectory myGitDirectory) (return ())
  createDirectory myGitDirectory
  createDirectory $ myGitDirectory ++ "/" ++ objectDirectory
  createDirectory $ myGitDirectory ++ "/" ++ refsDirectory
  B.writeFile (myGitDirectory ++ "/" ++ indexFile) ""

addCommand :: [String] -> IO ()
addCommand args = do
  let file = args !! 0
  content <- B.readFile file
  B.writeFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ (contentHashFileName content)) content
  writeIndex file content

writeIndex :: String -> ByteString -> IO ()
writeIndex file content = do
  objects <- readIndexObjects
  unless (Prelude.any (\x -> file == objectName x) objects) (addObjectToIndex objects file content)

addObjectToIndex :: [Object] -> String -> ByteString -> IO ()
addObjectToIndex objects file content = do
  writeObjectsToIndex (object:objects)
  where object = Object{
    objectPerm = [7,5,5],
    objectType = "file",
    objectHash = contentHashFileName content,
    objectName = file
    }

writeObjectsToIndex :: [Object] -> IO ()
writeObjectsToIndex objects = do
  Prelude.writeFile (myGitDirectory ++ "/" ++ indexFile) content
  where content = L.intercalate "\n" (Prelude.map objectToString objects)

objectToString :: Object -> String
objectToString o = do
  L.intercalate " " [permToString o, objectType o, objectHash o, objectName o]
  where
    permToString o = L.foldl (\x y -> x ++ (show y)) "" (objectPerm o)

readIndexObjects :: IO [Object]
readIndexObjects = do
  content <- Prelude.readFile (myGitDirectory ++ "/" ++ indexFile)
  let linesOfFiles = [x | x <- lines content, x /= ""]
  return (Prelude.map parseToObject linesOfFiles)

parseToObject :: String -> Object
parseToObject content = do
  let cols = splitOn " " content
      perm :: String -> [Int]
      perm p = Prelude.map (read . pure :: Char -> Int) p
  Object{
    objectPerm = perm (cols !! 0),
    objectType = cols !! 1,
    objectHash = cols !! 2,
    objectName = cols !! 3
    }

contentHashFileName :: ByteString -> String
contentHashFileName content = decode $ unpack $ hex $ SHA1.hash content

---

commitCommand :: [String] -> IO ()
commitCommand args = do
  object <- readIndexObjects
  treeHash <- writeTree object
  writeCommit treeHash
  clearIndex
  return ()

writeTree :: [Object] -> IO String
writeTree objects = do
  content <- B.readFile (myGitDirectory ++ "/" ++ indexFile)
  let hash = contentHashFileName content
  B.writeFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ hash) content
  return hash

writeCommit :: String -> IO ()
writeCommit treeHash = do
  parentCommitHash <- currentRef
  parentTreeHash <- Prelude.readFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ parentCommitHash)
  if parentTreeHash == treeHash then return ()
  else do
    let content = (treeHash ++ "\n" ++ parentCommitHash)
        commitHash = contentHashFileName $ pack $ encode content
    Prelude.writeFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ commitHash) content
    Prelude.writeFile (myGitDirectory ++ "/" ++ headFile) commitHash
    return ()

clearIndex :: IO ()
clearIndex = Prelude.writeFile (myGitDirectory ++ "/" ++ indexFile) ""

headFile :: String
headFile = "refs/heads/master"

currentRef :: IO String
currentRef = Prelude.readFile (myGitDirectory ++ "/" ++ headFile)

statusCommand :: [String] -> IO ()
statusCommand args = do
  print =<< readIndexObjects

logsCommand :: [String] -> IO ()
logsCommand args = do
  commitHash <- Prelude.readFile (myGitDirectory ++ "/" ++ headFile)
  print =<< readCommitHash commitHash

readCommitHash :: String -> IO [Object]
readCommitHash commitHash = do
  commit <- Prelude.readFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ commitHash)
  let treeHash = (lines commit) !! 0
  tree <- Prelude.readFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ treeHash)
  -- TODO: duplicate code
  let linesOfFiles = lines tree
  return (Prelude.map parseToObject linesOfFiles)

catFileCommand :: [String] -> IO ()
catFileCommand args = do
  print =<< Prelude.readFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ (args !! 0))