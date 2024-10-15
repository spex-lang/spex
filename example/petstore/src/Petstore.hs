{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Petstore where

import Control.Monad.IO.Class
import Data.Aeson
import Data.IORef
import Data.List
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import Servant

------------------------------------------------------------------------

type PetstoreAPI =
  "pet"    :> Capture "petId" Int :> Get '[JSON] Pet :<|>
  "pet"    :> ReqBody '[JSON] Pet :> Post '[JSON] () :<|>
  "health" :> Get '[JSON] () :<|>
  "_reset" :> Delete '[JSON] ()

data Pet = Pet
  { petId   :: Int
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
      liftIO (putStrLn ("getPet " ++ show pid))
      case find (\pet -> petId pet == pid) pets of
        Nothing  -> throwError err404
        Just pet -> return pet

    addPet :: Pet -> Handler ()
    addPet pet = liftIO $ do
      putStrLn ("addPet: " ++ show pet)
      modifyIORef store (pet :)

    health :: Handler ()
    health = return ()

    reset :: Handler ()
    reset = liftIO (writeIORef store [])

petstoreAPI :: Proxy PetstoreAPI
petstoreAPI = Proxy

app :: IORef [Pet] -> Application
app store = serve petstoreAPI (server store)

libMain :: IO ()
libMain = do
  store <- newIORef [Pet 1 "apa"]
  putStrLn "Running petstore on localhost:8080"
  run 8080 (app store)

