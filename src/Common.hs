module Common where

import Data.List
import Data.List.Split
import System.IO as IO
import System.IO.Strict as SIO(readFile, run)

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

data Config = Config{
  configAuthor :: Maybe String,
  configRemotes :: [Remote]
} deriving (Show, Eq)

data Remote = Remote{
  remoteUrl :: String,
  remoteName :: String
} deriving (Show, Eq)

myGitDirectory :: String
myGitDirectory = ".mygit"

refsDirectory :: String
refsDirectory = "refs"

objectDirectory :: String
objectDirectory = "objects"

tagsDirectory :: String
tagsDirectory = "tags"

headsDirectory :: String
headsDirectory = "heads"

indexFile :: String
indexFile = "index"

configFile :: String
configFile = "config"

headFile :: String
headFile = "HEAD"

masterBranchName :: String
masterBranchName = "master"

objectFilePath :: String -> String
objectFilePath file = myGitDirectory ++ "/" ++ objectDirectory ++ "/" ++ file

branchFilePath :: String -> String
branchFilePath branchName = myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ headsDirectory ++ "/" ++ branchName


readCommitHash :: String -> IO Commit
readCommitHash commitHash =
  if commitHash == "" then
    return Root
  else do
    commit <- IO.readFile $ objectFilePath commitHash
    let _:commitLines = lines commit
        treeHash = head commitLines
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

readTreeObjects :: String -> IO [Object]
readTreeObjects treeHash = do
  content <- SIO.run $ SIO.readFile $ objectFilePath treeHash
  let _:linesOfFiles = [x | x <- lines content, x /= ""]
  mapM parseToObject linesOfFiles

parseToObject :: String -> IO Object
parseToObject content = do
  let cols = splitOn " " content
      objectType = cols !! 1
  objectChildren <- if objectType == "tree" then readTreeObjects $ cols !! 2 else return []
  return Object{
    objectPerm = perm $ head cols,
    objectType = cols !! 1,
    objectHash = cols !! 2,
    objectName = cols !! 3,
    objectChildren = objectChildren
    }
  where
    perm :: String -> [Int]
    perm = map (read . pure :: Char -> Int)

color :: String -> String -> String
color c src =
  if c == "" then
    src
  else
    (case c of
      "red" -> "\x1b[31m"
      "green" -> "\x1b[32m"
      "yellow" -> "\x1b[33m"
      "blue" -> "\x1b[34m"
      "bold" -> "\x1b[1m"
      ) ++ src ++ "\x1b[m"

-- one > another
diff :: String -> [Object] -> [Object] -> [Diff]
diff path one another =
  one >>= diff' another path
  where
    diff' :: [Object] -> String -> Object -> [Diff]
    diff' objects path obj = do
      let objName = objectName obj
          target = find (\x -> objName == objectName x) objects
      if objectType obj == "file" then
        createFileDiff path target obj
      else createTreeDiff path target obj
    createFileDiff :: String -> Maybe Object -> Object -> [Diff]
    createFileDiff path Nothing obj =
      [
        Diff{
          diffFile = path ++ "/" ++ objectName obj,
          diffType = "add",
          diffBefore = "",
          diffAfter = objectHash obj
          }
        ]
    createFileDiff path (Just target) obj
      | objectHash obj /= objectHash target =
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
    createTreeDiff path Nothing obj =
      [
        Diff{
          diffFile = path ++ "/" ++ objectName obj,
          diffType = "add",
          diffBefore = "",
          diffAfter = objectHash obj
          }
        ]
    createTreeDiff path (Just target) obj
      | objectHash obj /= objectHash target =
        diff (path ++ "/" ++ objectName obj) (objectChildren obj) (objectChildren target)
      | otherwise = []

isRoot :: Commit -> Bool
isRoot Commit{} = False
isRoot Root = True
