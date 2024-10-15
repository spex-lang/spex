{-# LANGUAGE OverloadedStrings #-}

module Spex.Monad
  ( module Spex.Monad

  -- * Re-export
  , liftIO
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader hiding (asks)
import qualified Control.Monad.Trans.Reader as Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import qualified Network.HTTP.Client as Http

import Spex.CommandLine.Ansi
import Spex.CommandLine.ArgParser
import Spex.Syntax
import Spex.Syntax.Operation

------------------------------------------------------------------------

newtype App a = App { unApp :: ReaderT AppEnv (ExceptT AppError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp appEnv = runExceptT . flip runReaderT appEnv . unApp

asks :: (AppEnv -> a) -> App a
asks f = App (Reader.asks f)

------------------------------------------------------------------------

data AppEnv = AppEnv
  { specFile   :: FilePath
  , deployment :: Deployment
  , config     :: Config
  , logger     :: Logger
  }

data Config = Config
  { numTests :: Word
  , seed     :: Maybe Int
  }


newAppEnv :: CmdLineArgs -> IO AppEnv
newAppEnv args = do
  hasAnsi <- hasAnsiSupport

  let logger_ | not hasAnsi || args.nonInteractive = noAnsiLogger
              | otherwise = ansiLogger

  return AppEnv
    { specFile   = args.specFilePath
    , deployment = Deployment (HostPort args.host args.port)
                     (HealthCheckPath args.health) (ResetPath args.reset)
    , config     = Config (fromMaybe 100 args.numTests) args.seed
    , logger     = logger_
    }

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

quietLogger :: Logger -> Logger
quietLogger l = l { loggerInfo = const (return ()) }

verboseLogger :: Logger -> Logger
verboseLogger = undefined

info :: String -> App ()
info s = do
  l <- asks logger
  liftIO (l.loggerInfo s)

logError :: String -> App ()
logError s = do
  l <- asks logger
  liftIO (l.loggerError s)

------------------------------------------------------------------------

data AppError
  = ReadSpecFileError IOError
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

displayAppError :: FilePath -> AppError -> String
displayAppError spec = \case
  ReadSpecFileError _e    -> "Couldn't open specification file: " <> spec
  ParserError e              -> "Parse error: " <> e
  InvalidDeploymentUrl url   -> "Invalid deployment URL: " <> url
  HttpClientException op e   -> displayHttpException op e
  HttpClientDecodeError op body e -> "Couldn't decode the response of:\n\n    " <> displayOp op <> "\n\nfrom the body of the request: '" <> BS8.unpack body <> "'\n\nThe error being: " <> e
  HttpClientUnexpectedStatusCode _ _ -> "HTTP client returned 1xx or 3xx"
  TestFailure e seed         -> "Test failure: " <> e <>
                                "\nUse --seed " <> show seed <> " to reproduce"

displayHttpException :: Op -> HttpException -> String
displayHttpException op (HttpExceptionRequest _req content) = case content of
  ConnectionFailure _someException -> "Couldn't connect to host."
  StatusCodeException resp _ -> displayOp op <> "\n" <> show (Http.responseStatus resp)
  err -> show err
displayHttpException _op InvalidUrlException {} =
  error "impossible: already handled by InvalidDeploymentUrl"
