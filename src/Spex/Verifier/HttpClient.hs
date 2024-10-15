{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.HttpClient where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Method as Http

import Spex.Monad
import Spex.Syntax
import Spex.Syntax.Operation
import Spex.Syntax.Type
import Spex.Syntax.Value
import Spex.Verifier.Codec.Json

------------------------------------------------------------------------

data HttpClient = HttpClient
  { manager     :: Http.Manager
  , baseRequest :: Http.Request
  }

newHttpClient :: Deployment -> App HttpClient
newHttpClient (Deployment (HostPort host port) _health _reset) = do
  mgr <- liftIO (Http.newManager Http.defaultManagerSettings)
  let url  = host ++ ":" ++ show port
  req <- pure (Http.parseUrlThrow url) ?> InvalidDeploymentUrl url
  return (HttpClient mgr req)

httpRequest :: HttpClient -> Op -> App Value
httpRequest client op = do
  let req = client.baseRequest
              { Http.method = toHttpMethod op.method
              , Http.path   = toHttpPath op.path
              }
  resp <- liftIO (try (Http.httpLbs req client.manager))
            <?> HttpClientException op
  pure (decode (LBS.toStrict (Http.responseBody resp)))
    <?> HttpClientDecodeError op

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
