{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( initCommand,
      addCommand,
      commitCommand,
      logCommand,
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
import System.IO.Strict as SIO
import qualified Crypto.Hash.SHA1 as SHA1

data Object = Object{
  objectPerm :: [Int],
  objectType :: String,
  objectHash :: String,
  objectName :: String,
  objectChildren :: [Object]
} deriving Show

data Commit = Commit{
  commitAuthor :: String,
  commitMessage :: String,
  commitHash :: String,
  commitParent :: Commit
} | Root deriving Show

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
  ifM (doesDirectoryExist myGitDirectory) (removeDirectory myGitDirectory) (return ())
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
    objectName = file,
    objectChildren = []
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
  content <- SIO.run $ SIO.readFile (myGitDirectory ++ "/" ++ indexFile)
  let linesOfFiles = [x | x <- lines content, x /= ""]
  Prelude.mapM parseToObject linesOfFiles

readTreeObjects :: String -> IO [Object]
readTreeObjects file = do
  content <- SIO.run $ SIO.readFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ file)
  let linesOfFiles = [x | x <- lines content, x /= ""]
  Prelude.mapM parseToObject linesOfFiles

parseToObject :: String -> IO Object
parseToObject content = do
  let cols = splitOn " " content
      perm :: String -> [Int]
      perm p = Prelude.map (Prelude.read . pure :: Char -> Int) p
      objectType = cols !! 1
  objectChildren <- if objectType == "Tree" then readTreeObjects $ cols !! 2 else return []
  return Object{
    objectPerm = perm (cols !! 0),
    objectType = cols !! 1,
    objectHash = cols !! 2,
    objectName = cols !! 3,
    objectChildren = objectChildren
    }

contentHashFileName :: ByteString -> String
contentHashFileName content = decode $ unpack $ hex $ SHA1.hash content

---

commitCommand :: [String] -> IO ()
commitCommand args = do
  currentCommitHash <- currentRef
  currentCommit <- Prelude.readFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ currentCommitHash)
  let commits = lines currentCommit
  tree <- readTreeObjects $ commits !! 0
  let newTree = replaceTree tree tree
  objects <- readIndexObjects
  if (L.length objects) == 0 then do
    Prelude.print "no stage object"
  else do
    if (L.length args /= 2) then do
      Prelude.print "argument number should be 2"
    else do
      let author = args !! 0
          message = args !! 1
      treeHash <- writeTree newTree
      writeCommit treeHash author message
      clearIndex
      return ()

replaceTree :: [Object] -> [Object] -> [Object]
replaceTree tree replaceTree = do
  L.map (replaceObject replaceTree) tree
  where
    replaceObject :: [Object] -> Object -> Object
    replaceObject replaceTree object = do
      if inObject replaceTree (objectName object) then do
        readObject $ objectName object
      else object
    inObject :: [Object] -> String -> Bool
    inObject objects name = do
      L.map (\o -> objectName o == name) objects

writeTree :: [Object] -> IO String
writeTree objects = do
  content <- B.readFile (myGitDirectory ++ "/" ++ indexFile)
  let hash = contentHashFileName content
  B.writeFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ hash) content
  return hash

writeCommit :: String -> String -> String -> IO ()
writeCommit treeHash author message = do
  parentCommitHash <- currentRef
  if parentCommitHash /= "" then do
    parentCommit <- Prelude.readFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ parentCommitHash)
    let commits = lines parentCommit
    if commits !! 0 == treeHash then do
      Prelude.print "same commit"
      return ()
    else do
      let content = L.intercalate "\n" [treeHash, parentCommitHash, author, message]
          commitHash = contentHashFileName $ pack $ encode content
      Prelude.writeFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ commitHash) content
      Prelude.writeFile (myGitDirectory ++ "/" ++ headFile) commitHash
      return ()
  else do
    let content = L.intercalate "\n" [treeHash, "", author, message]
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
  Prelude.print =<< readIndexObjects

logCommand :: [String] -> IO ()
logCommand args = do
  commitHash <- Prelude.readFile (myGitDirectory ++ "/" ++ headFile)
  commit <- readCommitHash commitHash
  Prelude.putStrLn $ renderCommit commit

readCommitHash :: String -> IO Commit
readCommitHash commitHash = do
  if commitHash == "" then do
    return Root
  else do
    commit <- Prelude.readFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ commitHash)
    let commitLines = lines commit
        treeHash = commitLines !! 0
        parentHash = commitLines !! 1
        author = commitLines !! 2
        message = commitLines !! 3
    parentCommit <- readCommitHash parentHash
    return Commit{
      commitHash = commitHash,
      commitAuthor = author,
      commitMessage = message,
      commitParent = parentCommit
    }

--  tree <- Prelude.readFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ treeHash)
--  -- TODO: duplicate code
--  let linesOfFiles = lines tree
--  return (Prelude.map parseToObject linesOfFiles)

catFileCommand :: [String] -> IO ()
catFileCommand args = do
  Prelude.putStrLn =<< Prelude.readFile (myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ (args !! 0))

renderCommit :: Commit -> String
renderCommit Root = ""
renderCommit Commit{..} = do
  let commitLog = L.intercalate "\n" [
        "commit " ++ commitHash,
        "Author: " ++ commitAuthor,
        "Message: " ++ commitMessage
        ]
  commitLog ++ "\n\n" ++ renderCommit commitParent
