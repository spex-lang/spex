{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Petstore where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.IORef
import Data.List
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import Servant

------------------------------------------------------------------------

type PetstoreAPI =
  "pet" :> Capture "petId" Int :> Get '[JSON] Pet
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
server store = getPet :<|> addPet :<|> health :<|> reset
  where
    getPet :: Int -> Handler Pet
    getPet pid = do
      pets <- liftIO (readIORef store)
      case find (\pet -> petId pet == pid) pets of
        Nothing -> throwError err404
        Just pet -> return pet

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
app store = serve petstoreAPI (server store)

libMain :: Port -> IO ()
libMain port = do
  store <- newIORef [Pet 1 "apa"]
  putStrLn $ "Running petstore on localhost:" <> show port
  run port (app store)
