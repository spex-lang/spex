{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Petstore where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.List
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

------------------------------------------------------------------------

type PetstoreAPI =
  "pet" :> Capture "petId" Int :> Get '[JSON] Pet
    :<|> "pet" :> "badJson" :> Capture "petId" Int :> Get '[JSON] Pet
    :<|> "pet" :> ReqBody '[JSON] Pet :> Post '[JSON] ()
    :<|> "health" :> Get '[JSON] ()
    :<|> "_reset" :> Delete '[JSON] ()

data Pet = Pet
  { petId :: Int
  , petName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Pet
instance FromJSON Pet

server :: IORef [Pet] -> Server PetstoreAPI
server store = getPet :<|> badGetPet :<|> addPet :<|> health :<|> reset
  where
    getPet :: Int -> Handler Pet
    getPet pid = do
      pets <- liftIO (readIORef store)
      case find (\pet -> petId pet == pid) pets of
        Nothing -> throwError err404
        Just pet -> return pet

    -- XXX: We'll make it do bad stuff in the middleware.
    badGetPet :: Int -> Handler Pet
    badGetPet = getPet

    addPet :: Pet -> Handler ()
    addPet pet = do
      pets <- liftIO (readIORef store)
      when (pet `elem` pets) $
        throwError err409 {errBody = "Pet already exists"}
      liftIO (writeIORef store (pet : pets))

    health :: Handler ()
    health = return ()

    reset :: Handler ()
    reset = liftIO (writeIORef store [])

petstoreAPI :: Proxy PetstoreAPI
petstoreAPI = Proxy

app :: IORef [Pet] -> Application
app store = faultInject (serve petstoreAPI (server store))

faultInject :: Middleware
faultInject baseApp req respond0
  | "badJson" `elem` pathInfo req =
      baseApp req $ \response -> do
        let status = responseStatus response
        let headers = responseHeaders response
        body <- responseBody response
        respond0 $ responseLBS status headers (LBS.drop 1 body)
  | otherwise = baseApp req respond0

responseBody :: Response -> IO LBS.LazyByteString
responseBody resp =
  let (_status, _headers, body) = responseToStream resp
  in  body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        BSB.toLazyByteString <$> readIORef content

libMain :: Port -> IO ()
libMain port = do
  store <- newIORef [Pet 1 "apa"]
  run port (app store)
