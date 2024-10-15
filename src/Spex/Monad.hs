{-# LANGUAGE OverloadedStrings #-}

module Spex.Monad
  ( module Spex.Monad

  -- * Re-export
  , liftIO
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import qualified Network.HTTP.Client as Http

import Spex.CommandLine.Ansi
import Spex.Syntax.Operation

------------------------------------------------------------------------

newtype App a = App { unApp :: ReaderT AppEnv (ExceptT AppError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp env = runExceptT . flip runReaderT env . unApp

------------------------------------------------------------------------

data AppEnv = AppEnv
  { logger :: Logger }

defaultAppEnv :: AppEnv
defaultAppEnv = AppEnv noAnsiLogger

data Logger = Logger
  { loggerInfo  :: String -> IO ()
  , loggerError :: String -> IO ()
  }

noAnsiLogger :: Logger
noAnsiLogger = Logger
  { loggerInfo  = putStrLn . ("i " ++)
  , loggerError = putStrLn . ("Error: " ++ )
  }

ansiLogger :: Logger
ansiLogger = Logger
  { loggerInfo  = putStrLn . (cyan "i " ++)
  , loggerError = putStrLn . (boldRed "Error: " ++ )
  }

newAppEnv :: IO AppEnv
newAppEnv = do
  hasAnsi <- hasAnsiSupport
  if hasAnsi
  then return defaultAppEnv { logger = ansiLogger }
  else return defaultAppEnv

info :: String -> App ()
info s = do
  l <- App (asks logger)
  liftIO (l.loggerInfo s)

logError :: String -> App ()
logError s = do
  l <- App (asks logger)
  liftIO (l.loggerError s)

------------------------------------------------------------------------

data AppError
  = ReadSpecFileError FilePath IOError
  | ParserError String
  | InvalidDeploymentUrl String
  | HttpClientException Op HttpException
  | HttpClientDecodeError Op ByteString String
  | HttpClientUnexpectedStatusCode Int ByteString
  | TestFailure String Int

throwA :: AppError -> App e
throwA e = App (ReaderT (const (throwE e)))

infixl 8 <?>
(<?>) :: App (Either e a) -> (e -> AppError) -> App a
m <?> f = m >>= \case
  Left err -> throwA (f err)
  Right x  -> return x

(?>) :: App (Maybe a) -> AppError -> App a
m ?> err = m >>= \case
  Nothing -> throwA err
  Just x  -> return x

displayAppError :: AppError -> String
displayAppError = \case
  ReadSpecFileError fp _e    -> "Couldn't open specification file: " <> fp
  ParserError e              -> "Parse error: " <> e
  InvalidDeploymentUrl url   -> "Invalid deployment URL: " <> url
  HttpClientException op e   -> displayHttpException op e
  HttpClientDecodeError op body e -> "Couldn't decode the response of:\n\n    " <> displayOp op <> "\n\nfrom the body of the request: '" <> BS8.unpack body <> "'\n\nThe error being: " <> e
  TestFailure e seed         -> "Test failure: " <> e <>
                                "\nUse --seed " <> show seed <> " to reproduce"

displayHttpException :: Op -> HttpException -> String
displayHttpException op (HttpExceptionRequest _req content) = case content of
  ConnectionFailure _someException -> "Couldn't connect to host."
  StatusCodeException resp _ -> displayOp op <> "\n" <> show (Http.responseStatus resp)
  err -> show err
displayHttpException _op InvalidUrlException {} =
  error "impossible: already handled by InvalidDeploymentUrl"
