{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( initCommand,
      addCommand,
      commitCommand,
      logCommand,
      statusCommand,
      catFileCommand,
      treeCommand,
      diffCommand,
      branchCommand,
      checkoutCommand,
      tagCommand,
      remoteCommand,
      configCommand,
      pushCommand,
      serverCommand,
      Object(..),
      replaceTree,
      searchTree,
      searchFile,
    ) where

import Data.ByteString as B(readFile, writeFile, ByteString(..), pack, unpack)
import Data.Hex
import Data.Maybe
import Data.Either
import Data.List as L
import Data.List.Split
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(..))
import Data.Yaml(decodeFileEither, encodeFile, ParseException(..))
import System.Directory
import Control.Monad
import Control.Monad.Extra
import Codec.Binary.UTF8.String
import System.IO.Strict as SIO(readFile, run)
import System.IO as IO
import Text.Read as R
import Text.Show.Pretty
import qualified Crypto.Hash.SHA1 as SHA1
import GHC.Generics
import Util
import Tcp
import Common

initCommand :: [String] -> IO ()
initCommand args = do
  ifM (doesDirectoryExist myGitDirectory) (removeDirectory myGitDirectory) (return ())
  createDirectory myGitDirectory
  createDirectory $ myGitDirectory ++ "/" ++ objectDirectory
  createDirectory $ myGitDirectory ++ "/" ++ refsDirectory
  createDirectory $ myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ headsDirectory
  createDirectory $ myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ tagsDirectory
  IO.writeFile (myGitDirectory ++ "/" ++ configFile) "remotes: []"
  IO.writeFile (myGitDirectory ++ "/" ++ indexFile) ""
  IO.writeFile (myGitDirectory ++ "/" ++ headFile) (refsDirectory ++ "/" ++ headsDirectory ++ "/" ++ masterBranchName)
  IO.writeFile (myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ headsDirectory ++ "/" ++ masterBranchName) ""

addCommand :: [String] -> IO ()
addCommand args = do
  let file = L.head args
  content <- B.readFile file
  B.writeFile (objectFilePath $ contentHashFileName content) content
  writeIndex file content

writeIndex :: String -> ByteString -> IO ()
writeIndex file content = do
  objects <- readIndexObjects
  unless (L.any (\x -> file == objectName x) objects) (addObjectToIndex objects file content)

addObjectToIndex :: [Object] -> String -> ByteString -> IO ()
addObjectToIndex objects file content =
  writeObjectsToIndex (object:objects)
  where object = Object{
    objectPerm = [6, 4, 4],
    objectType = "file",
    objectHash = contentHashFileName content,
    objectName = file,
    objectChildren = []
    }

writeObjectsToIndex :: [Object] -> IO ()
writeObjectsToIndex objects =
  IO.writeFile (myGitDirectory ++ "/" ++ indexFile) content
  where content = L.intercalate "\n" (L.map objectToString objects)

objectToString :: Object -> String
objectToString o =
  unwords [permToString o, objectType o, objectHash o, objectName o]
  where
    permToString o = L.foldl (\x y -> x ++ show y) "" (objectPerm o)

readIndexObjects :: IO [Object]
readIndexObjects = do
  content <- SIO.run $ SIO.readFile (myGitDirectory ++ "/" ++ indexFile)
  let linesOfFiles = [x | x <- lines content, x /= ""]
  mapM parseToObject linesOfFiles

contentHashFileName :: ByteString -> String
contentHashFileName content = decode $ unpack $ hex $ SHA1.hash content

calculateHash :: [Object] -> String
calculateHash tree =
  decode $ unpack $ hex $ SHA1.hash $ pack $ encode ("tree\n" ++ L.intercalate "\n" (treeString tree))
  where
    treeString :: [Object] -> [String]
    treeString = L.map objectToString

commitCommand :: [String] -> IO ()
commitCommand args = do
  ref <- currentRef
  currentCommitHash <- IO.readFile $ myGitDirectory ++ "/" ++ ref
  tree <- if currentCommitHash /= "" then do
    currentCommit <- IO.readFile $ objectFilePath currentCommitHash
    let _:commits = lines currentCommit
    readTreeObjects $ L.head commits
  else return []
  objects <- readIndexObjects
  if L.null objects then
    IO.hPrint IO.stderr "no stage object"
  else
    if L.length args /= 2 then
      IO.hPrint IO.stderr "argument number should be 2"
    else do
      let author = L.head args
          message = args !! 1
      newTree <- foldM convertTree tree objects
      mapM_ writeTree' newTree
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
appendTree tree indexedObject (path:paths) =
  if objectType indexedObject == "file" then indexedObject:tree
  else [] -- searchTree tree indexedObject

replaceTree :: [Object] -> Object -> [String] -> [Object]
replaceTree tree indexedObject p@(path:paths)
  | L.null paths = if searchFile path tree then
      L.map (replaceObject indexedObject p) tree else
      indexedObject{objectName = path}:tree
  | searchTree path tree = L.map (replaceObject indexedObject p) tree
  | otherwise = createTree indexedObject p:tree
  where
    replaceObject :: Object -> [String] -> Object -> Object
    replaceObject indexedObject (path:paths) object
      | objectType object == "file" =
        if path == objectName object then indexedObject{objectName = path} else object
      | objectType object == "tree" =
        if path == objectName object then do
          let newTree = replaceTree (objectChildren object) indexedObject paths
          object{objectHash = calculateHash newTree, objectChildren = newTree}
        else object

createTree :: Object -> [String] -> Object
createTree indexedObject [path] = indexedObject{objectName = path}
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
      when (newHash /= objectHash treeObject) $
        IO.hPrint IO.stderr "tree hash not match"

writeTree :: [Object] -> IO String
writeTree objects = do
  let content = "tree\n" ++ L.intercalate "\n" (L.map objectToString objects)
      encodedContent = pack $ encode content
      hash = contentHashFileName encodedContent
  B.writeFile (objectFilePath hash) encodedContent
  return hash

writeCommit :: String -> String -> String -> IO ()
writeCommit treeHash author message = do
  ref <- currentRef
  let refPath = myGitDirectory ++ "/" ++ ref
  parentCommitHash <- IO.readFile refPath
  if parentCommitHash /= "" then do
    parentCommit <- IO.readFile $ objectFilePath parentCommitHash
    let commits = lines parentCommit
    if L.head commits == treeHash then do
      IO.hPrint IO.stderr "same commit"
      return ()
    else do
      let content = "commit\n" ++ L.intercalate "\n" [treeHash, parentCommitHash, author, message]
          commitHash = contentHashFileName $ pack $ encode content
      IO.writeFile (objectFilePath commitHash) content
      IO.writeFile refPath commitHash
  else do
    let content = "commit\n" ++ L.intercalate "\n" [treeHash, "", author, message]
        commitHash = contentHashFileName $ pack $ encode content
    IO.writeFile (objectFilePath commitHash) content
    IO.writeFile refPath commitHash
    return ()

clearIndex :: IO ()
clearIndex = IO.writeFile (myGitDirectory ++ "/" ++ indexFile) ""

currentRef :: IO String
currentRef = SIO.run $ SIO.readFile (myGitDirectory ++ "/" ++ headFile)

statusCommand :: [String] -> IO ()
statusCommand args =
  pPrint =<< readIndexObjects

logCommand :: [String] -> IO ()
logCommand args = do
  ref <- IO.readFile (myGitDirectory ++ "/" ++ headFile)
  commitHash <- IO.readFile (myGitDirectory ++ "/" ++ ref)
  commit <- readCommitHash commitHash
  IO.putStrLn $ renderCommit commit

catFileCommand :: [String] -> IO ()
catFileCommand args =
  IO.putStrLn =<< IO.readFile (objectFilePath $ L.head args)

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
  currentCommitHash <- if L.null args then do
    ref <- currentRef
    IO.readFile $ myGitDirectory ++ "/" ++ ref
    else return $ L.head args
  currentCommit <- IO.readFile $ objectFilePath currentCommitHash
  let _:commits = lines currentCommit
  tree <- readTreeObjects $ L.head commits
  mapM_ (printTree "") tree
--  pPrint tree

resetCommand :: [String] -> IO ()
resetCommand args = do
  let hash = L.head args
  content <- IO.readFile $ objectFilePath hash
  let contentType = L.head commitLines
      commitLines = lines content
  if contentType == "commit" then do
    ref <- currentRef
    IO.writeFile (myGitDirectory ++ "/" ++ ref) hash
  else IO.hPrint IO.stderr ("object type shoule be commit, but " ++ contentType)

diffCommand :: [String] -> IO ()
diffCommand args = do
  let one = L.head args
      another = args !! 1
  ref <- currentRef
  currentCommitHash <- IO.readFile $ myGitDirectory ++ "/" ++ ref
  oneTree <- do
    oneCommit <- IO.readFile $ objectFilePath one
    readTreeObjects $ (lines oneCommit) !! 1
  anotherTree <- do
    anotherCommit <- IO.readFile $ objectFilePath another
    readTreeObjects $ (lines anotherCommit) !! 1
  mapM_ printDiff $ diff "." oneTree anotherTree


printDiff :: Diff -> IO ()
printDiff diff = do
  content <- diffContent diff
  mapM_ IO.putStrLn [
    color "bold" (diffType diff ++ " " ++ diffFile diff),
    content,
    ""
    ]
  where
    diffContent :: Diff -> IO String
    diffContent diff
      | diffType diff == "add" = do
        content <- SIO.run $ SIO.readFile $ objectFilePath $ diffAfter diff
        return $ color "green" content
      | diffType diff == "update" = do
        before <- IO.readFile $ objectFilePath $ diffBefore diff
        after <- IO.readFile $ objectFilePath $ diffAfter diff
        return $ color "red" before ++ "\n" ++ color "green" after

printTree :: String -> Object -> IO ()
printTree indent obj
  | objectType obj == "file" =
    IO.putStrLn $ indent ++ objectName obj
  | objectType obj == "tree" = do
    IO.putStrLn $ color "bold" (indent ++ objectName obj ++ "/")
    mapM_ (printTree (indent ++ "  ")) (objectChildren obj)

branchCommand :: [String] -> IO ()
branchCommand args =
  if L.null args then
    IO.putStrLn =<< currentRef
  else do
    let branch = L.head args
        branchFileName = myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ headsDirectory ++ "/" ++ branch
    ifM (doesFileExist branchFileName) (handleError branch) (createNewBranch branch branchFileName)
    where
      handleError :: String -> IO ()
      handleError branch =
        IO.hPutStrLn IO.stderr $ "branch already exists: " ++ branch
      createNewBranch :: String -> String -> IO ()
      createNewBranch branch branchFileName = do
        ref <- currentRef
        currentCommitHash <- IO.readFile $ myGitDirectory ++ "/" ++ ref
        IO.writeFile branchFileName currentCommitHash
        IO.writeFile (myGitDirectory ++ "/" ++ headFile) (refsDirectory ++ "/" ++ headsDirectory ++ "/" ++ branch)

checkoutCommand :: [String] -> IO ()
checkoutCommand args = do
  let branch = L.head args
      branchFileName = myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ headsDirectory ++ "/" ++ branch
  ifM (doesFileExist branchFileName) (checkout branch branchFileName) (handleError branch)
    where
      handleError :: String -> IO ()
      handleError branch =
        IO.hPutStrLn IO.stderr $ "branch does not exist: " ++ branch
      checkout :: String -> String -> IO ()
      checkout branch branchFileName =
        IO.writeFile (myGitDirectory ++ "/" ++ headFile) (refsDirectory ++ "/" ++ headsDirectory ++ "/" ++ branch)

tagCommand :: [String] -> IO ()
tagCommand args =
  if L.null args then do
    directories <- listDirectory $ myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ tagsDirectory
    mapM_ IO.putStrLn directories
  else do
    let tag = L.head args
        tagFileName = myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ tagsDirectory ++ "/" ++ tag
    ifM (doesFileExist tagFileName) (handleError tag) (createNewBranch tag tagFileName)
    where
      handleError :: String -> IO ()
      handleError tag =
        IO.hPutStrLn IO.stderr $ "tag already exists: " ++ tag
      createNewBranch :: String -> String -> IO ()
      createNewBranch tag tagFileName = do
        ref <- currentRef
        currentCommitHash <- IO.readFile $ myGitDirectory ++ "/" ++ ref
        IO.writeFile tagFileName currentCommitHash

configCommand :: [String] -> IO ()
configCommand args
  | L.null args = do
    config <- decodeFileEither $ myGitDirectory ++ "/" ++ configFile
    printConfig config
  | L.length args == 2 = do
    config <- decodeFileEither $ myGitDirectory ++ "/" ++ configFile
    let name = L.head args
        value = args !! 1
        newConfig = setConfig config name value
    encodeFile (myGitDirectory ++ "/" ++ configFile) newConfig
  | otherwise = hPutStrLn stderr "arguments should be 0 or 2"

printConfig :: Either ParseException Config -> IO ()
printConfig (Left err) = hPrint stderr err
printConfig (Right config) = do
  let author = configAuthor config
  putStr "author: "
  maybe (return ()) IO.putStrLn author
  mapM_ printRemote $ configRemotes config

printRemote :: Remote -> IO ()
printRemote remote = IO.putStrLn $ remoteName remote ++ " => " ++ remoteUrl remote

setConfig :: Either ParseException Config -> String -> String -> Config
setConfig (Left err) name value
  | name == "author" = Config{configAuthor = Just value, configRemotes = []}
  | otherwise = Config{configAuthor = Nothing, configRemotes = []}
setConfig (Right config) name value
  | name == "author" = config{configAuthor = Just value}
  | otherwise = config

remoteCommand :: [String] -> IO ()
remoteCommand args
  | null args = do
    config <- decodeFileEither $ myGitDirectory ++ "/" ++ configFile
    if isLeft config then do
      let Left err = config
      hPrint stderr err
    else do
      let Right right = config
      mapM_ printRemote $ configRemotes right
  | length args == 2 = do
    configEither <- decodeFileEither $ myGitDirectory ++ "/" ++ configFile
    if isLeft configEither then do
      let Left err = configEither
      hPrint stderr err
    else do
      let name = head args
          url = args !! 1
          Right config = configEither
          remotes = configRemotes config
          newRemotes = if any (\c -> remoteName c == name) remotes then
            map (\r -> if remoteName r == name then r{remoteUrl = url} else r) remotes
          else
            Remote{remoteName = name, remoteUrl = url}:remotes
      encodeFile (myGitDirectory ++ "/" ++ configFile) config{configRemotes = newRemotes}
  | otherwise = hPutStrLn stderr "arguments should be 0 or 2"

pushCommand :: [String] -> IO ()
pushCommand args
  | length args == 2 = do
    let dest = head args
        branch = args !! 1
    pushToServer dest branch
  | otherwise = return ()

serverCommand :: [String] -> IO ()
serverCommand args = runServer

-- get ls-remote # => refsのリスト
-- なければ新規作成
-- あればそこにpush

deriveJSON defaultOptions { fieldLabelModifier = firstLower . drop 6 } ''Config
deriveJSON defaultOptions { fieldLabelModifier = firstLower . drop 6 } ''Remote
