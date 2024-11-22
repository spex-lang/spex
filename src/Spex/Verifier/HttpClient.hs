{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.HttpClient where

import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.String (fromString)
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types qualified as Http

import Spex.Monad
import Spex.Syntax
import Spex.Verifier.Codec.Json

------------------------------------------------------------------------

data HttpClient = HttpClient
  { manager :: Http.Manager
  , baseRequest :: Http.Request
  }

newHttpClient :: Deployment -> App HttpClient
newHttpClient (Deployment (HostPort host port) _health _reset) = do
  mgr <- liftIO (Http.newManager Http.defaultManagerSettings)
  let url = BS8.unpack host ++ ":" ++ show port
  req <- pure (Http.parseRequest url) ?> InvalidDeploymentUrl url

  return $
    HttpClient
      mgr
      req
        { Http.requestHeaders =
            [ ("Accept", "application/json")
            , ("Content-Type", "application/json")
            ]
        }

data Response
  = Ok2xx {statusCode :: Int, body :: ByteString}
  | ClientError4xx {statusCode :: Int, body :: ByteString}
  | ServerError5xx {statusCode :: Int, body :: ByteString}

httpRequest :: HttpClient -> Op -> App Response
httpRequest client op = do
  let req =
        client.baseRequest
          { Http.method = toHttpMethod op.method
          , Http.requestHeaders =
              Http.requestHeaders client.baseRequest ++ map toHeader op.headers
          , Http.path = toHttpPath op.path
          , Http.requestBody = toHttpBody op.body
          }
  trace $ "httpRequest, req: " <> show req
  case op.body of
    Nothing -> return ()
    Just body -> trace $ "httpRequest, body: " <> LBS8.unpack (encode body)
  resp <-
    liftIO (try (Http.httpLbs req client.manager))
      <?> HttpClientException op
  trace $ "httpRequest, resp: " <> show resp
  let status = Http.responseStatus resp
      body = LBS.toStrict (Http.responseBody resp)
  if
    | Http.statusIsSuccessful status -> do
        return (Ok2xx status.statusCode body)
    | Http.statusIsClientError status ->
        return
          ( ClientError4xx
              status.statusCode
              (status.statusMessage <> if BS8.null body then "" else ": " <> body)
          )
    | Http.statusIsServerError status ->
        return
          ( ServerError5xx
              status.statusCode
              (status.statusMessage <> if BS8.null body then "" else ": " <> body)
          )
    | otherwise ->
        throwA
          ( HttpClientUnexpectedStatusCode
              status.statusCode
              status.statusMessage
          )

toHttpMethod :: Method -> Http.Method
toHttpMethod Get = Http.methodGet
toHttpMethod Post = Http.methodPost
toHttpMethod Put = Http.methodPut
toHttpMethod Delete = Http.methodDelete

toHeader :: Header -> Http.Header
toHeader (Header name mVal) = case BS8.split ':' name of
  [name', val] ->
    ( fromString (BS8.unpack name')
    , val <> case mVal of
        Nothing -> ""
        Just val' -> LBS.toStrict (encode val')
    )
  _otherwise -> error "toHeader: impossible"

toHttpPath :: [PathSegment Value] -> ByteString
toHttpPath = BS8.intercalate "/" . map aux
  where
    aux (Path p) = p
    aux (Hole _x (IntV i)) = BS8.pack (show i)
    aux (Hole _x (StringV txt)) = Text.encodeUtf8 txt
    aux (Hole _x ty) = error ("toHttpPath: " <> show ty)
-- ^ XXX: Do other types make sense to turn into paths?

toHttpBody :: Maybe Value -> Http.RequestBody
toHttpBody Nothing = Http.RequestBodyBS ""
toHttpBody (Just val) = Http.RequestBodyLBS (encode val)
