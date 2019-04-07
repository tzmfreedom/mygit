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
      treeCommand,
      diffCommand,
      Object(..),
      replaceTree,
      searchTree,
      searchFile,
    ) where

import Data.ByteString as B
import Data.Hex
import Data.Maybe
import Data.List as L
import Data.List.Split
import System.Directory
import Control.Monad
import Control.Monad.Extra
import Codec.Binary.UTF8.String
import System.IO.Strict as SIO
import System.IO as IO
import Text.Show.Pretty
import qualified Crypto.Hash.SHA1 as SHA1

data Object = Object{
  objectPerm :: [Int],
  objectType :: String,
  objectHash :: String,
  objectName :: String,
  objectChildren :: [Object]
} deriving (Show, Eq)

data Commit = Commit{
  commitAuthor :: String,
  commitMessage :: String,
  commitHash :: String,
  commitParent :: Commit
} | Root deriving (Show, Eq)

data Diff = Diff{
  diffFile :: String,
  diffType :: String,
  diffBefore :: String,
  diffAfter :: String
} deriving (Show, Eq)

myGitDirectory :: String
myGitDirectory = ".mygit"

refsDirectory :: String
refsDirectory = "refs"

objectDirectory :: String
objectDirectory = "objects"

indexFile :: String
indexFile = "index"

headFile :: String
headFile = "refs/heads/master"

objectFilePath :: String -> String
objectFilePath file = myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ file

initCommand :: [String] -> IO ()
initCommand args = do
  ifM (doesDirectoryExist myGitDirectory) (removeDirectory myGitDirectory) (return ())
  createDirectory myGitDirectory
  createDirectory $ myGitDirectory ++ "/" ++ objectDirectory
  createDirectory $ myGitDirectory ++ "/" ++ refsDirectory
  createDirectory $ myGitDirectory ++ "/" ++ refsDirectory ++ "/heads"
  B.writeFile (myGitDirectory ++ "/" ++ indexFile) ""
  B.writeFile (myGitDirectory ++ "/" ++ refsDirectory ++ "/heads/master") ""

addCommand :: [String] -> IO ()
addCommand args = do
  let file = args !! 0
  content <- B.readFile file
  B.writeFile (objectFilePath $ contentHashFileName content) content
  writeIndex file content

writeIndex :: String -> ByteString -> IO ()
writeIndex file content = do
  objects <- readIndexObjects
  unless (Prelude.any (\x -> file == objectName x) objects) (addObjectToIndex objects file content)

addObjectToIndex :: [Object] -> String -> ByteString -> IO ()
addObjectToIndex objects file content = do
  writeObjectsToIndex (object:objects)
  where object = Object{
    objectPerm = [6, 4, 4],
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
readTreeObjects treeHash = do
  content <- SIO.run $ SIO.readFile $ objectFilePath treeHash
  let _:linesOfFiles = [x | x <- lines content, x /= ""]
  Prelude.mapM parseToObject linesOfFiles

parseToObject :: String -> IO Object
parseToObject content = do
  let cols = splitOn " " content
      objectType = cols !! 1
  objectChildren <- if objectType == "tree" then readTreeObjects $ cols !! 2 else return []
  return Object{
    objectPerm = perm (cols !! 0),
    objectType = cols !! 1,
    objectHash = cols !! 2,
    objectName = cols !! 3,
    objectChildren = objectChildren
    }
  where
    perm :: String -> [Int]
    perm p = Prelude.map (Prelude.read . pure :: Char -> Int) p

contentHashFileName :: ByteString -> String
contentHashFileName content = decode $ unpack $ hex $ SHA1.hash content

calculateHash :: [Object] -> String
calculateHash tree = do
  decode $ unpack $ hex $ SHA1.hash $ pack $ encode ("tree\n" ++ (L.intercalate "\n" $ treeString tree))
  where
    treeString :: [Object] -> [String]
    treeString = L.map objectToString

commitCommand :: [String] -> IO ()
commitCommand args = do
  currentCommitHash <- currentRef
  tree <- if currentCommitHash /= "" then do
    currentCommit <- Prelude.readFile $ objectFilePath currentCommitHash
    let _:commits = lines currentCommit
    readTreeObjects $ commits !! 0
  else return []
  objects <- readIndexObjects
  if (L.length objects) == 0 then do
    IO.hPrint IO.stderr "no stage object"
  else do
    if (L.length args /= 2) then do
      IO.hPrint IO.stderr "argument number should be 2"
    else do
      let author = args !! 0
          message = args !! 1
      newTree <- foldM convertTree tree objects
      mapM writeTree' newTree
      treeHash <- writeTree newTree
      writeCommit treeHash author message
      clearIndex
      return ()
  where
    convertTree :: [Object] -> Object -> IO [Object]
    convertTree tree obj = do
      let filePaths = splitOn "/" (objectName obj)
      return (replaceTree tree obj filePaths)

searchTree :: String -> [Object] -> Bool
searchTree path = L.any (\x -> objectType x == "tree" && objectName x == path)
searchFile :: String -> [Object] -> Bool
searchFile path = L.any (\x -> objectType x == "file" && objectName x == path)

appendTree :: [Object] -> Object -> [String] -> [Object]
appendTree tree indexedObject (path:paths) = do
  if objectType indexedObject == "file" then indexedObject:tree
  else [] -- searchTree tree indexedObject

replaceTree :: [Object] -> Object -> [String] -> [Object]
replaceTree tree indexedObject p@(path:paths) = do
  -- file
  if L.length paths == 0 then do
    if searchFile path tree then do
      L.map (replaceObject indexedObject p) tree
    else indexedObject{objectName = path}:tree
  -- tree
  else do
    if searchTree path tree then do
      L.map (replaceObject indexedObject p) tree
    else do
      (createTree indexedObject p):tree
  where
    replaceObject :: Object -> [String] -> Object -> Object
    replaceObject indexedObject (path:paths) object
      | objectType object == "file" = do
        if path == objectName object then indexedObject{objectName = path} else object
      | objectType object == "tree" = do
        if path == objectName object then do
          let newTree = replaceTree (objectChildren object) indexedObject paths
          object{objectHash = calculateHash newTree, objectChildren = newTree}
        else object

createTree :: Object -> [String] -> Object
createTree indexedObject (path:[]) = indexedObject{objectName = path}
createTree indexedObject (path:paths) = do
  let newTree = Object{
    objectName = path,
    objectHash = "",
    objectType = "tree",
    objectChildren = [createTree indexedObject paths],
    objectPerm = [7, 5, 5]
    }
  let newHash = calculateHash $ objectChildren newTree
  newTree{objectHash = newHash}

writeTree' :: Object -> IO ()
writeTree' treeObject
  | objectType treeObject == "file" = return ()
  | objectType treeObject == "tree" = do
    let file = objectFilePath $ objectHash treeObject
    ifM (doesFileExist file) (return ()) $ do
      newHash <- writeTree $ objectChildren treeObject
      if newHash /= objectHash treeObject then do
        IO.hPrint IO.stderr "tree hash not match"
      else return ()

writeTree :: [Object] -> IO String
writeTree objects = do
  let content = "tree\n" ++ (L.intercalate "\n" $ L.map objectToString objects)
      encodedContent = pack $ encode content
      hash = contentHashFileName encodedContent
  B.writeFile (objectFilePath hash) encodedContent
  return hash

writeCommit :: String -> String -> String -> IO ()
writeCommit treeHash author message = do
  parentCommitHash <- currentRef
  if parentCommitHash /= "" then do
    parentCommit <- Prelude.readFile $ objectFilePath parentCommitHash
    let commits = lines parentCommit
    if commits !! 0 == treeHash then do
      IO.hPrint IO.stderr "same commit"
      return ()
    else do
      let content = "commit\n" ++ L.intercalate "\n" [treeHash, parentCommitHash, author, message]
          commitHash = contentHashFileName $ pack $ encode content
      Prelude.writeFile (objectFilePath commitHash) content
      Prelude.writeFile (myGitDirectory ++ "/" ++ headFile) commitHash
      return ()
  else do
    let content = "commit\n" ++ L.intercalate "\n" [treeHash, "", author, message]
        commitHash = contentHashFileName $ pack $ encode content
    Prelude.writeFile (objectFilePath commitHash) content
    Prelude.writeFile (myGitDirectory ++ "/" ++ headFile) commitHash
    return ()

clearIndex :: IO ()
clearIndex = Prelude.writeFile (myGitDirectory ++ "/" ++ indexFile) ""

currentRef :: IO String
currentRef = SIO.run $ SIO.readFile (myGitDirectory ++ "/" ++ headFile)

statusCommand :: [String] -> IO ()
statusCommand args = do
  pPrint =<< readIndexObjects

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
    commit <- Prelude.readFile $ objectFilePath commitHash
    let _:commitLines = lines commit
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

catFileCommand :: [String] -> IO ()
catFileCommand args = do
  Prelude.putStrLn =<< Prelude.readFile (objectFilePath $ args !! 0)

renderCommit :: Commit -> String
renderCommit Root = ""
renderCommit Commit{..} = do
  let commitLog = L.intercalate "\n" [
        color "yellow" ("commit " ++ commitHash),
        "Author: " ++ commitAuthor,
        "",
        "    " ++ commitMessage
        ]
  commitLog ++ "\n\n" ++ renderCommit commitParent

treeCommand :: [String] -> IO ()
treeCommand args = do
  currentCommitHash <- if L.length args == 0 then currentRef else return (args !! 0)
  currentCommit <- Prelude.readFile $ objectFilePath currentCommitHash
  let _:commits = lines currentCommit
  tree <- readTreeObjects $ commits !! 0
  mapM (printTree "") tree
  return ()
--  pPrint tree

resetCommand :: [String] -> IO ()
resetCommand args = do
  let hash = args !! 0
  content <- Prelude.readFile $ objectFilePath hash
  let contentType = commitLines !! 0
      commitLines = lines content
  if contentType == "commit" then do
    Prelude.writeFile (myGitDirectory ++ "/" ++ headFile) hash
  else IO.hPrint IO.stderr ("object type shoule be commit, but " ++ contentType)

diffCommand :: [String] -> IO ()
diffCommand args = do
  let one = args !! 0
      another = args !! 1
  currentCommitHash <- currentRef
  oneTree <- do
    oneCommit <- Prelude.readFile $ objectFilePath one
    readTreeObjects $ (lines oneCommit) !! 0
  anotherTree <- do
    anotherCommit <- Prelude.readFile $ objectFilePath another
    readTreeObjects $ (lines anotherCommit) !! 0
  mapM printDiff $ diff "." oneTree anotherTree
  return ()

-- one > another
diff :: String -> [Object] -> [Object] -> [Diff]
diff path one another = do
  one >>= (diff' another path)
  where
    diff' :: [Object] -> String -> Object -> [Diff]
    diff' objects path obj = do
      let objName = objectName obj
          target = L.find (\x -> objName == objectName x) objects
      if objectType obj == "file" then do
        createFileDiff path target obj
      else createTreeDiff path target obj
    createFileDiff :: String -> Maybe Object -> Object -> [Diff]
    createFileDiff path Nothing obj = do
      [
        Diff{
          diffFile = path ++ "/" ++ objectName obj,
          diffType = "add",
          diffBefore = "",
          diffAfter = objectHash obj
          }
        ]
    createFileDiff path (Just target) obj
      | objectHash obj /= objectHash target = do
        [
          Diff {
            diffFile = path ++ "/" ++ objectName obj,
            diffType = "update",
            diffBefore = objectHash target,
            diffAfter = objectHash obj
            }
          ]
      | otherwise = []
    createTreeDiff :: String -> Maybe Object -> Object -> [Diff]
    createTreeDiff path Nothing obj = do
      [
        Diff{
          diffFile = path ++ "/" ++ objectName obj,
          diffType = "add",
          diffBefore = "",
          diffAfter = objectHash obj
          }
        ]
    createTreeDiff path (Just target) obj
      | objectHash obj /= objectHash target = do
        diff (path ++ "/" ++ objectName obj) (objectChildren obj) (objectChildren target)
      | otherwise = []

printDiff :: Diff -> IO ()
printDiff diff = do
  content <- diffContent diff
  mapM IO.putStrLn [
    color "bold" (diffType diff ++ " " ++ diffFile diff),
    content,
    ""
    ]
  return ()
  where
    diffContent :: Diff -> IO String
    diffContent diff
      | diffType diff == "add" = do
        content <- SIO.run $ SIO.readFile $ objectFilePath $ diffAfter diff
        return $ color "green" content
      | diffType diff == "update" = do
        before <- Prelude.readFile $ objectFilePath $ diffBefore diff
        after <- Prelude.readFile $ objectFilePath $ diffAfter diff
        return $ color "red" before ++ "\n" ++ color "green" after

color :: String -> String -> String
color c src = do
  if c == "" then do
    src
  else do
    (case c of
      "red" -> "\x1b[31m"
      "green" -> "\x1b[32m"
      "yellow" -> "\x1b[33m"
      "blue" -> "\x1b[34m"
      "bold" -> "\x1b[1m"
      ) ++ src ++ "\x1b[m"

printTree :: String -> Object -> IO ()
printTree indent obj
  | objectType obj == "file" = do
    IO.putStrLn $ (indent ++ objectName obj)
  | objectType obj == "tree" = do
    IO.putStrLn $ color "bold" (indent ++ objectName obj ++ "/")
    mapM (printTree (indent ++ "  ")) (objectChildren obj)
    return ()
