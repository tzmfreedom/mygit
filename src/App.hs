{-# LANGUAGE OverloadedStrings #-}

module App where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Text.Parsec as TP
import Text.Parsec.String
import Text.Parsec.Char
import Text.EDE
import Codec.Binary.UTF8.String as CBS
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as C
import Data.Aeson as DA
import System.Directory
import System.IO as IO
import Data.Text as T
import Data.Text.Lazy as TL
import Data.List as L
import Common

type Handler = Request -> IO Response

appCommand :: [String] -> IO ()
appCommand args =
  run 8080 server
  where
    server :: Application
    server req respond = do
      let handler = getHandler req
      respond =<< handler
    getHandler :: Handler
    getHandler req =
      case TP.parse (parseRoute req) "" (CBS.decode $ BS.unpack path) of
        Left err -> error "ParserError"
        Right ls -> ls req
      where
        path :: C.ByteString
        path = rawPathInfo req
    parseRoute :: Request -> Parser Handler
    parseRoute req =
      try (parseTop req) <|> try (parseBranch req) <|> try (parseCommit req) <|> try (parseBlob req)

    parseTop :: Request -> Parser Handler
    parseTop req = do
      string "/"
      eof
      return (case requestMethod req of
        "GET" -> root
        _ -> notFound
        )
    parseBranch :: Request -> Parser Handler
    parseBranch req = do
      string "/branches"
      eof *> return (case method of
        "GET" -> branchIndex
        _ -> notFound
        ) <|> do
        char '/'
        branchId <- many1 anyChar
        return (case method of
          "GET" -> branchShow branchId
          _ -> notFound
          )
      where
        method = requestMethod req

    parseCommit :: Request -> Parser Handler
    parseCommit req = do
      string "/commits/"
      commitId <- many1 anyChar
      return (case method of
        "GET" -> commitShow commitId
        _ -> notFound
        )
      where
        method = requestMethod req

    parseBlob :: Request -> Parser Handler
    parseBlob req = do
      string "/blob/"
      commitId <- many1 hexDigit
      char '/'
      path <- many1 anyChar
      return (case method of
        "GET" -> blobShow commitId path
        _ -> notFound
        )
      where
        method = requestMethod req
    render :: Either String TL.Text -> IO Response
    render (Left err) = error "hoge"
    render (Right body) =
      return $ responseLBS status200 [] $ BSL.fromStrict $ BS.pack $ CBS.encode $ TL.unpack body
    root :: Handler
    root _ = return $ responseFile status200 [] "./tmpl/index.html" Nothing
    branchIndex :: Handler
    branchIndex _ = do
      r <- eitherParseFile "./tmpl/branches/index.ede"
      env <- getEnv
      let eitherBody = r >>= (`eitherRender` env)
      render eitherBody
      where
        getEnv :: IO DA.Object
        getEnv = do
          files <- listDirectory $ myGitDirectory ++ "/refs/heads"
          return (fromPairs [ "branches" .= files ] :: DA.Object)
    branchShow :: String -> Handler
    branchShow branchId _ = do
      r <- eitherParseFile "./tmpl/branches/show.ede"
      env <- getEnv
      let eitherBody = r >>= (`eitherRender` env)
      render eitherBody
      where
        getEnv :: IO DA.Object
        getEnv = do
          commitHash <- IO.readFile (myGitDirectory ++ "/" ++ refsDirectory ++ "/" ++ headsDirectory ++ "/" ++ branchId)
          commit <- readCommitHash commitHash
          let commits = createCommits commit
          files <- listDirectory $ myGitDirectory ++ "/refs/heads/"
          return (fromPairs [ "branch" .= branchId, "commits" .= commits ] :: DA.Object)
        createCommits :: Commit -> [DA.Object]
        createCommits Root = []
        createCommits commit =
          fromPairs [
            "hash" .= commitHash commit,
            "author" .= commitAuthor commit,
            "message" .= commitMessage commit
            ]:createCommits (commitParent commit)
    commitShow :: String -> Handler
    commitShow commitId _ = do
      r <- eitherParseFile "./tmpl/commits/show.ede"
      env <- getEnv
      let eitherBody = r >>= (`eitherRender` env)
      render eitherBody
      where
        render :: Either String TL.Text -> IO Response
        render (Left err) = error "hoge"
        render (Right body) =
          return $ responseLBS status200 [] $ BSL.fromStrict $ BS.pack $ CBS.encode $ TL.unpack body
        getEnv :: IO DA.Object
        getEnv = do
          commit <- IO.readFile $ objectFilePath commitId
          let treeHash = L.lines commit !! 1
          tree <- readTreeObjects treeHash
          return (fromPairs [ "commit" .= commitId, "objects" .= L.map treeToObject tree ] :: DA.Object)
        treeToObject :: Common.Object -> DA.Object
        treeToObject obj =
          fromPairs [
            "name" .= objectName obj,
            "hash" .= objectHash obj,
            "type" .= objectType obj
            ]
    blobShow :: String -> String -> Handler
    blobShow commitId path _ = do
      commit <- IO.readFile $ objectFilePath commitId
      tree <- readTreeObjects $ L.lines commit !! 1
      let target = L.find (\x -> objectName x == path) tree
      r <- eitherParseFile "./tmpl/blob/show.ede"
      env <- getEnv target
      let eitherBody = r >>= (`eitherRender` env)
      render eitherBody
      where
        getEnv :: Maybe Common.Object -> IO DA.Object
        getEnv (Just blob) = do
          content <- IO.readFile $ objectFilePath $ objectHash blob
          return (fromPairs [
            "name" .= objectName blob,
            "hash" .= objectHash blob,
            "type" .= objectType blob,
            "content" .= content
            ] :: DA.Object)
    notFound :: Handler
    notFound _ = return $ responseFile status200 [] "./tmpl/index.html" Nothing
