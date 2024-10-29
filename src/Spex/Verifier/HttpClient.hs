{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.HttpClient where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http

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
  let url  = BS8.unpack host ++ ":" ++ show port
  req <- pure (Http.parseRequest url) ?> InvalidDeploymentUrl url

  return $ HttpClient mgr req { Http.requestHeaders =
    [ ("Accept",       "application/json")
    , ("Content-type", "application/json")
    ]}

data Response
  = Ok2xx ByteString
  | ClientError4xx Int
  | ServerError5xx Int

httpRequest :: HttpClient -> Op -> App Response
httpRequest client op = do
  let req = client.baseRequest
              { Http.method      = toHttpMethod op.method
              , Http.path        = toHttpPath op.path
              , Http.requestBody = toHttpBody op.body
              }
  trace $ "httpRequest, req: " <> show req
  case op.body of
    Nothing   -> return ()
    Just body -> trace $ "httpRequest, body: " <> LBS8.unpack (encode body)
  resp <- liftIO (try (Http.httpLbs req client.manager))
            <?> HttpClientException op
  trace $ "httpRequest, resp: " <> show resp
  let status = Http.responseStatus resp
  if | Http.statusIsSuccessful status -> do
         let body = LBS.toStrict (Http.responseBody resp)
         return (Ok2xx body)
     | Http.statusIsClientError status ->
         return (ClientError4xx status.statusCode)
     | Http.statusIsServerError status ->
         return (ServerError5xx status.statusCode)
     | otherwise ->
         throwA (HttpClientUnexpectedStatusCode
                   status.statusCode status.statusMessage)

toHttpMethod :: Method -> Http.Method
toHttpMethod Get  = Http.methodGet
toHttpMethod Post = Http.methodPost

toHttpPath :: [PathSegment Value] -> ByteString
toHttpPath = BS8.intercalate "/" . map aux
  where
    aux (Path p)                = p
    aux (Hole _x (IntV i))      = BS8.pack (show i)
    aux (Hole _x (StringV txt)) = Text.encodeUtf8 txt
    aux (Hole _x ty)            = error ("toHttpPath: " <> show ty)
      -- ^ XXX: Do other types make sense to turn into paths?

toHttpBody :: Maybe Value -> Http.RequestBody
toHttpBody Nothing    = Http.RequestBodyBS ""
toHttpBody (Just val) = Http.RequestBodyLBS (encode val)
