{-# LANGUAGE OverloadedStrings #-}

module Tcp where

import qualified Control.Exception as E
import Control.Concurrent (forkFinally)
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Foreign.C.Types
import System.Directory
import Codec.Binary.UTF8.String as CBS
import System.IO as IO
import Data.ByteString.Lazy(fromStrict, toStrict)
import Data.ByteString.Base64 as B64
import Data.Maybe
import Data.List as L(find, length, drop)
import Codec.Archive.Zip
import Control.Monad.Extra
import Common

port :: String
port = "3000"

pushToServer :: String -> IO ()
pushToServer dest = do
  addr <- resolve "127.0.0.1" port
  E.bracket (open addr) close talk
  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock
    talk sock = do
      sendAll sock "ls-remote\0heads"
      heads <- recv sock 1024
      sendAll sock "ls-remote\0tags"
      tags <- recv sock 1024
      ref <- S.readFile (myGitDirectory ++ "/" ++ headFile)
      let refs = map parseRef (C.lines heads) ++ if tags == "NOTHING" then [] else map parseRef (C.lines tags)
          ref' = find (\(x,y) -> absoluteRefsPath x == ref) refs
          targetCommitHash = CBS.decode $ S.unpack $ maybe "" snd ref'
          absoluteRefsPath = S.append $ S.pack $ CBS.encode (refsDirectory ++ "/")
      targetCommit <- findCommit targetCommitHash
      if not targetCommit then hPutStrLn stderr $ "cannot find commit: " ++ targetCommitHash
      else do
        commit <- currentCommit
        diff' <- findDiff commit targetCommitHash
        body <- createZipArchive diff'
        sendAll sock $ S.append "commit\0" body
    findDiff :: Commit -> String -> IO [Diff]
    findDiff commit toCommitHash
      | commitHash commit == toCommitHash = return []
      | isRoot $ commitParent commit = do
        let from = commitHash commit
        fromTree <- do
          fromCommit <- IO.readFile $ objectFilePath from
          readTreeObjects $ lines fromCommit !! 1
        return $ diffWithTree "." fromTree []
      | otherwise = do
        let from = commitHash commit
            to = commitHash $ commitParent commit
        fromTree <- do
          fromCommit <- IO.readFile $ objectFilePath from
          readTreeObjects $ lines fromCommit !! 1
        toTree <- do
          toCommit <- IO.readFile $ objectFilePath to
          readTreeObjects $ lines toCommit !! 1
        parentDiff <- findDiff (commitParent commit) toCommitHash
        commitDiff <- createCommitDiff from
        return $ commitDiff:diffWithTree "." fromTree toTree ++ parentDiff
        where
          createCommitDiff :: String -> IO Diff
          createCommitDiff from = do
            fromCommit <- IO.readFile $ objectFilePath from
            return Diff{
              diffFile = objectFilePath from,
              diffType = "add",
              diffBefore = "",
              diffAfter = lines fromCommit !! 1
              }
    findCommit :: String -> IO Bool
    findCommit targetCommitHash = do
      commit <- currentCommit
      return $ findCommit' commit targetCommitHash
      where
        findCommit' :: Commit -> String -> Bool
        findCommit' Root _  = True
        findCommit' commit targetCommitHash
          | targetCommitHash == "" = True
          | commitHash commit == targetCommitHash = True
          | otherwise = findCommit' (commitParent commit) targetCommitHash
    currentCommit :: IO Commit
    currentCommit = do
      ref <- IO.readFile (myGitDirectory ++ "/" ++ headFile)
      commitHash <- IO.readFile (myGitDirectory ++ "/" ++ ref)
      readCommitHash commitHash
    createZipArchive :: [Diff] -> IO S.ByteString
    createZipArchive diff = do
      ref <- IO.readFile (myGitDirectory ++ "/" ++ headFile)
      commitHash <- S.readFile (myGitDirectory ++ "/" ++ ref)
      entries <- mapM entry diff
      let refEntry = toEntry (myGitDirectory ++ "/" ++ ref) 0 $ fromStrict commitHash
          archive = Archive{zEntries = refEntry:entries, zSignature = Nothing, zComment = ""}
      return $ B64.encode $ toStrict $ fromArchive archive
      where
        entry :: Diff -> IO Entry
        entry d = do
          let path = objectFilePath $ diffAfter d
          content <- S.readFile path
          return $ toEntry path 0 $ fromStrict content
    parseRef :: S.ByteString -> (S.ByteString, S.ByteString)
    parseRef = toTuple . C.words
      where
        toTuple [x] = (x,"")
        toTuple [x,y] = (x,y)

runServer :: IO ()
runServer =
  withSocketsDo $ do
    addr <- resolve port
    E.bracket (open addr) close loop
  where
    resolve :: String -> IO AddrInfo
    resolve port = do
      let hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
      return addr
    open :: AddrInfo -> IO Socket
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      -- If the prefork technique is not used,
      -- set CloseOnExec for the security reasons.
      let fd = fdSocket sock
      setCloseOnExecIfNeeded fd
      bind sock (addrAddress addr)
      listen sock 10
      return sock
    loop :: Socket -> IO ()
    loop sock = forever $ do
      (conn, peer) <- accept sock
      putStrLn $ "Connection from " ++ show peer
      void $ forkFinally (talk conn) (\_ -> close conn)
    talk :: Socket -> IO ()
    talk conn = do
      msg <- recv conn 1024
      unless (S.null msg) $ do
        let method:body = S.split 0 msg
        dispatch method (head body) conn
        talk conn
    dispatch :: S.ByteString -> S.ByteString -> Socket -> IO ()
    dispatch method body conn
      | method == "ls-remote" = do
        let basedir = if body == "heads" then headsDirectory else tagsDirectory
        files <- listDirectory $ myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ basedir
        formattedRefs <- mapM (formatRef . addDirectoryPath basedir) files
        if null formattedRefs then sendAll conn "NOTHING"
        else sendAll conn $ S.intercalate "\n" formattedRefs
      | method == "commit" = do
        let Right decoded = B64.decode body
        extractFilesFromArchive [OptDestination tmpDirectory, OptVerbose, OptRecursive] $ toArchive $ fromStrict decoded
        copyRecursively tmpDirectory
        where
          copyRecursively :: FilePath -> IO ()
          copyRecursively dirpath = do
            files <- listDirectory dirpath
            mapM_ (copy dirpath) files
            where
              copy :: FilePath -> FilePath -> IO ()
              copy dirpath filepath = do
                let abspath= dirpath ++ "/" ++ filepath
                ifM (doesFileExist abspath) (copyFile abspath $ L.drop (L.length tmpDirectory + 1) abspath) $ do
                  files <- listDirectory abspath
                  mapM_ (copy abspath) files

    addDirectoryPath :: String -> String -> String
    addDirectoryPath dir path = dir ++ "/" ++ path
    formatRef :: FilePath -> IO S.ByteString
    formatRef file = do
      refHash <- IO.readFile $ myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ file
      return $ S.pack . CBS.encode $ file ++ ' ':refHash

tmpDirectory :: FilePath
tmpDirectory = "./tmp"