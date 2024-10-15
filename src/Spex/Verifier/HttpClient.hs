{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.HttpClient where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Method as Http

import Spex.Monad
import Spex.Syntax
import Spex.Syntax.Operation
import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------

data HttpClient = HttpClient
  { manager     :: Http.Manager
  , baseRequest :: Http.Request
  }

newHttpClient :: Deployment -> App HttpClient
newHttpClient (Deployment (HostPort host port) _health _reset) = do
  mgr <- liftIO (Http.newManager Http.defaultManagerSettings)
  let url  = host ++ ":" ++ show port
  req <- pure (Http.parseRequest url) ?> InvalidDeploymentUrl url
  return (HttpClient mgr req)

data Response
  = Ok2xx ByteString
  | ClientError4xx
  | ServerError5xx

httpRequest :: HttpClient -> Op -> App Response
httpRequest client op = do
  let req = client.baseRequest
              { Http.method = toHttpMethod op.method
              , Http.path   = toHttpPath op.path
              }
  resp <- liftIO (try (Http.httpLbs req client.manager))
            <?> HttpClientException op
  let status = Http.responseStatus resp
  if | Http.statusIsSuccessful status -> do
         let body = LBS.toStrict (Http.responseBody resp)
         return (Ok2xx body)
     | Http.statusIsClientError status -> return ClientError4xx
     | Http.statusIsServerError status -> return ServerError5xx
     | otherwise -> throwA (HttpClientUnexpectedStatusCode status.statusCode status.statusMessage)

toHttpMethod :: Method -> Http.Method
toHttpMethod Get  = Http.methodGet
toHttpMethod Post = Http.methodPost

toHttpPath :: [PathSegment Value] -> ByteString
toHttpPath = go
  where
    go []                            = ""
    go (Path p                :  ps) = p <> "/" <> go ps
    go (Hole _x (IntV i)      :  ps) = BS8.pack (show i) <> "/" <> go ps
    go (Hole _x (StringV txt) :  ps) = Text.encodeUtf8 txt <> "/" <> go ps
    go (Hole _x ty            : _ps) = error ("toHttpPath: " <> show ty)
      -- ^ XXX: Do other types make sense to turn into paths?
