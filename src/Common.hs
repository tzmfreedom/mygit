module Common where

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
