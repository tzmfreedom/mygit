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
import Codec.Binary.UTF8.String
import System.IO as IO
import Data.ByteString.Lazy(fromStrict)
import Codec.Archive.Zip
import Common

port :: String
port = "3000"

pushToServer dest ref = do
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
      msg <- recv sock 1024
      C.putStrLn msg
      sendAll sock "ls-remote\0tags"
      msg <- recv sock 1024
      C.putStrLn msg
      diff <- findDiff "" ""
      body <- createZipArchive diff
      sendAll sock $ S.append "commit\0" body
    findDiff :: String -> String -> IO [Diff]
    findDiff from to = do
      ref <- IO.readFile (myGitDirectory ++ "/" ++ headFile)
      commitHash <- IO.readFile (myGitDirectory ++ "/" ++ ref)
      commit <- readCommitHash commitHash
      fromTree <- do
        fromCommit <- IO.readFile $ objectFilePath from
        readTreeObjects $ (lines fromCommit) !! 1
      toTree <- do
        toCommit <- IO.readFile $ objectFilePath to
        readTreeObjects $ (lines toCommit) !! 1
      return $ diff "." fromTree toTree

    createZipArchive :: [Diff] -> IO S.ByteString
    createZipArchive diff =
      return ""
      where
        entry :: Diff -> IO Entry
        entry d = do
          let path = objectFilePath $ diffAfter d
          content <- S.readFile path
          return $ toEntry path 0 $ fromStrict content
        content :: Diff -> IO String
        content = IO.readFile . objectFilePath . diffAfter

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
        sendAll conn $ S.intercalate "\n" formattedRefs
      | method == "commit" =
        return ()
    addDirectoryPath :: String -> String -> String
    addDirectoryPath dir path = dir ++ "/" ++ path
    formatRef :: FilePath -> IO S.ByteString
    formatRef file = do
      refHash <- IO.readFile $ myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ file
      return $ S.pack . encode $ file ++ ' ':refHash

