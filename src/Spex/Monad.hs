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
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import qualified Network.HTTP.Client as Http

import Spex.CommandLine.Ansi
import Spex.CommandLine.Option
import Spex.Syntax
import Spex.Verifier.Generator.Env

------------------------------------------------------------------------

newtype App a = App { unApp :: ReaderT AppEnv (ExceptT AppError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp appEnv = runExceptT . flip runReaderT appEnv . unApp

asks :: (AppEnv -> a) -> App a
asks f = App (Reader.asks f)

local :: (AppEnv -> AppEnv) -> App a -> App a
local f (App m) = App (Reader.local f m)

------------------------------------------------------------------------

data AppEnv = AppEnv
  { logger      :: Logger
  }

newAppEnv :: Options -> IO AppEnv
newAppEnv opts = do
  hasAnsi <- hasAnsiSupport

  let logger' | not hasAnsi || opts.nonInteractive = noAnsiLogger
              | otherwise = ansiLogger
      logger'' = case opts.logging of
                   Verbose     True -> verboseLogger logger'
                   VeryVerbose True -> veryVerboseLogger logger'
                   Quiet       True -> quietLogger logger'
                   _otherwise       -> logger'

  return AppEnv
    { logger = logger''
    }

data Logger = Logger
  { loggerInfo  :: Bool -> String -> IO ()
  , loggerError :: String -> IO ()
  , loggerDebug :: String -> IO ()
  , loggerTrace :: String -> IO ()
  }

-- XXX: Check for unicode support, for checkmark?
noAnsiLogger :: Logger
noAnsiLogger = Logger
  { loggerInfo  = \b -> if b then putStrLn . ("✓ " ++)
                             else putStrLn . ("i " ++)
  , loggerError = putStrLn . ("Error: " ++ )
  , loggerDebug = \_s -> return ()
  , loggerTrace = \_s -> return ()
  }

ansiLogger :: Logger
ansiLogger = Logger
  { loggerInfo  = \b -> if b then putStrLn . (green "✓ " ++)
                             else putStrLn . (cyan "i " ++)
  , loggerError = putStrLn . ((boldRed "Error" ++ ": ") ++ )
  , loggerDebug = \_s -> return ()
  , loggerTrace = \_s -> return ()
  }

quietLogger :: Logger -> Logger
quietLogger l = l { loggerInfo = \_ _ -> return () }

verboseLogger :: Logger -> Logger
verboseLogger l =  l { loggerDebug = putStrLn . (faint "d " ++) }

veryVerboseLogger :: Logger -> Logger
veryVerboseLogger l = (verboseLogger l)
  { loggerTrace = putStrLn . (faint "t " ++) }

info :: String -> App ()
info s = do
  l <- asks logger
  liftIO (l.loggerInfo False s)

info_ :: String -> App ()
info_ s = do
  l <- asks logger
  liftIO (l.loggerInfo False ("\b\b  " ++ s))

logError :: String -> App ()
logError s = do
  l <- asks logger
  liftIO (l.loggerError s)

debug :: String -> App ()
debug s = do
  l <- asks logger
  liftIO (l.loggerDebug s)

debug_ :: String -> App ()
debug_ s = do
  l <- asks logger
  liftIO (l.loggerDebug ("\b\b  " ++ s))

trace :: String -> App ()
trace s = do
  l <- asks logger
  liftIO (l.loggerTrace s)

done :: String -> App ()
done s = do
  l <- asks logger
  liftIO (l.loggerInfo True s)

------------------------------------------------------------------------

data AppError
  = ReadSpecFileError IOError
  | ParserError String
  | ScopeError [(Pos, [Ann TypeId])]
  | InvalidDeploymentUrl String
  | HttpClientException Op HttpException
  | HttpClientDecodeError Op ByteString String
  | HttpClientUnexpectedStatusCode Int ByteString
  | HealthCheckFailed
  | ResetFailed
  | TestFailure [Op] Int String Int

throwA :: AppError -> App e
throwA e = App (ReaderT (const (throwE e)))

tryA :: App a -> App (Either AppError a)
tryA (App m) = App (ReaderT (tryE . runReaderT m))

infixl 8 <?>
(<?>) :: App (Either e a) -> (e -> AppError) -> App a
m <?> f = m >>= \case
  Left err -> throwA (f err)
  Right x  -> return x

(?>) :: App (Maybe a) -> AppError -> App a
m ?> err = m >>= \case
  Nothing -> throwA err
  Just x  -> return x

displayAppError :: FilePath -> LazyByteString -> AppError -> String
displayAppError spec lbs = \case
  ReadSpecFileError _e            -> "Couldn't open specification file: " <> spec
  ParserError e                   -> "Parse error: " <> e
  ScopeError tids                 -> uncurry (displayScopeError spec lbs) (head tids)
  InvalidDeploymentUrl url        -> "Invalid deployment URL: " <> url
  HttpClientException op e        -> displayHttpException op e
  HttpClientDecodeError op body e -> "Couldn't decode the response of:\n\n    " <>
                                     displayOp displayValue op <> "\n\nfrom the body of the request: '" <>
                                     BS8.unpack body <> "'\n\nThe error being: " <> e
  HttpClientUnexpectedStatusCode _ _ -> "HTTP client returned 1xx or 3xx"
  HealthCheckFailed                -> "Health check failed, make sure that the deployment is running."
  ResetFailed                      -> "Reset of the deploymnet failed, make sure that reset returns 2xx or exits with 0."
  TestFailure ops shrinks err seed -> "Test failure" <>
                                       (if shrinks > 0 then " (" <> show shrinks <> " shrinks)" else "") <>
                                      ":\n\n" <> displayOps ops <>
                                      err <>
                                      "\n\nUse --seed " <> show seed <> " to reproduce"

-- XXX: only displays one error at the time?!
displayScopeError :: FilePath -> LazyByteString -> Pos -> [Ann TypeId] -> String
displayScopeError fp lbs pos tids =
  let bs = LBS.toStrict lbs
      Ann pos' tid = head tids

      ls       = linesUtf8 bs
      (l, c, c') = case posLineCols bs [pos, pos'] of
                     [] -> error "displayScopeError: impossible"
                     (l0, c0) : (_l1, c1) : _ -> (l0, c0, c1)
      line     = if l < length ls then ls !! l else ""
      linum    = " " ++ show l
      lpad     = map (const ' ') linum

      prevLine  = if l - 1 >= 0 then ls !! (l - 1) else ""

  -- XXX: check $LANG for UTF8 support? otherwise use ascii box drawing?
  in "Scope error, the type " ++ displayTypeId tid ++ " isn't defined.\n\n" ++
     lpad   ++ " ┌── " ++ fp ++ ":" ++ show l ++ ":" ++ show c' ++ "\n" ++
     lpad   ++ " │\n" ++
     (if null prevLine then "" else lpad   ++ " | " ++ prevLine ++ "\n") ++
     linum  ++ " │ " ++ line ++ "\n" ++
     -- XXX: We should avoid ANSI here, maybe introduce a "FancyError" type and let LibMain display it?
     lpad   ++ " │ " ++ replicate c' ' ' ++ red (replicate (length (displayTypeId tid)) '^') ++ "\n\n" ++
     "Either define the type or mark it as abstract, in case it shouldn't be\ngenerated."

displayHttpException :: Op -> HttpException -> String
displayHttpException op (HttpExceptionRequest _req content) = case content of
  ConnectionFailure _someException -> "Couldn't connect to host."
  StatusCodeException resp _ -> displayOp displayValue op <> "\n" <> show (Http.responseStatus resp)
  err -> show err
displayHttpException _op InvalidUrlException {} =
  error "impossible: already handled by InvalidDeploymentUrl"
