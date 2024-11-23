{-# LANGUAGE OverloadedStrings #-}

module Spex.Monad (
  module Spex.Monad,

  -- * Re-export
  liftIO,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Network.HTTP.Client (
  HttpException (..),
  HttpExceptionContent (..),
 )
import Network.HTTP.Client qualified as Http
import System.IO

import Spex.CommandLine.Ansi
import Spex.CommandLine.Option
import Spex.PrettyPrinter
import Spex.Syntax

------------------------------------------------------------------------

newtype App a = App {unApp :: ReaderT AppEnv (ExceptT AppError IO) a}
  deriving (Functor, Applicative, Monad, MonadIO)

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp appEnv = runExceptT . flip runReaderT appEnv . unApp

asks :: (AppEnv -> a) -> App a
asks f = App (Reader.asks f)

local :: (AppEnv -> AppEnv) -> App a -> App a
local f (App m) = App (Reader.local f m)

------------------------------------------------------------------------

data AppEnv = AppEnv
  { logger :: Logger
  }

newAppEnv :: Options -> IO AppEnv
newAppEnv opts = do
  hasAnsi <- hasAnsiSupport

  (printer, flusher, closer) <- case opts.logFile of
    Nothing -> return (Text.hPutStrLn stderr, return (), return ())
    Just fp -> do
      -- XXX: can fail...
      h <- openFile fp WriteMode
      hSetBuffering h LineBuffering
      return (Text.hPutStrLn h, hFlush h, hClose h)

  let logger'
        | not hasAnsi || opts.nonInteractive =
            noAnsiLogger printer flusher closer
        | otherwise = ansiLogger printer flusher closer
      logger'' = case opts.logging of
        Verbose True -> verboseLogger printer logger'
        Trace True -> traceLogger printer logger'
        Quiet True -> quietLogger logger'
        _otherwise -> logger'

  return
    AppEnv
      { logger = logger''
      }

data Logger = Logger
  { loggerInfo :: Bool -> Text -> IO ()
  , loggerError :: Text -> IO ()
  , loggerDebug :: Text -> IO ()
  , loggerTrace :: Text -> IO ()
  , loggerFlush :: IO ()
  , loggerClose :: IO ()
  }

-- XXX: Check for unicode support, for checkmark?
noAnsiLogger :: (Text -> IO ()) -> IO () -> IO () -> Logger
noAnsiLogger printer flusher closer =
  Logger
    { loggerInfo = \b ->
        if b
          then printer . ("✓ " <>)
          else printer . ("i " <>)
    , loggerError = printer . ("Error: " <>)
    , loggerDebug = \_s -> return ()
    , loggerTrace = \_s -> return ()
    , loggerFlush = flusher
    , loggerClose = closer
    }

ansiLogger :: (Text -> IO ()) -> IO () -> IO () -> Logger
ansiLogger printer flusher closer =
  Logger
    { loggerInfo = \b ->
        if b
          then printer . (green "✓ " <>)
          else printer . (cyan "i " <>)
    , loggerError = printer . ((boldRed "Error" <> ": ") <>)
    , loggerDebug = \_s -> return ()
    , loggerTrace = \_s -> return ()
    , loggerFlush = flusher
    , loggerClose = closer
    }

quietLogger :: Logger -> Logger
quietLogger l = l {loggerInfo = \_ _ -> return ()}

verboseLogger :: (Text -> IO ()) -> Logger -> Logger
verboseLogger printer l = l {loggerDebug = printer . (faint "d " <>)}

traceLogger :: (Text -> IO ()) -> Logger -> Logger
traceLogger printer l =
  (verboseLogger printer l)
    { loggerTrace = printer . (faint "t " <>)
    }

info :: String -> App ()
info s = do
  l <- asks logger
  liftIO (l.loggerInfo False (Text.pack s))

info_ :: String -> App ()
info_ s = do
  l <- asks logger
  liftIO (l.loggerInfo False (Text.pack ("\b\b  " ++ s)))

logError :: String -> App ()
logError s = do
  l <- asks logger
  liftIO (l.loggerError (Text.pack s))

debug :: String -> App ()
debug s = do
  l <- asks logger
  liftIO (l.loggerDebug (Text.pack s))

debug_ :: String -> App ()
debug_ s = do
  l <- asks logger
  liftIO (l.loggerDebug (Text.pack ("\b\b  " ++ s)))

trace :: String -> App ()
trace s = do
  l <- asks logger
  liftIO (l.loggerTrace (Text.pack s))

done :: String -> App ()
done s = do
  l <- asks logger
  liftIO (l.loggerInfo True (Text.pack s))

flushLogger :: App ()
flushLogger = do
  l <- asks logger
  liftIO l.loggerFlush

closeLogger :: App ()
closeLogger = do
  l <- asks logger
  liftIO l.loggerClose

------------------------------------------------------------------------

data AppError
  = ReadSpecFileError IOError
  | ParserError String
  | ScopeError [(Pos, [Ann TypeId])]
  | InvalidDeploymentUrl String
  | HttpClientException Op HttpException
  | HttpClientUnexpectedStatusCode Int ByteString
  | HealthCheckFailed
  | ResetFailed

throwA :: AppError -> App e
throwA e = App (ReaderT (const (throwE e)))

tryA :: App a -> App (Either AppError a)
tryA (App m) = App (ReaderT (tryE . runReaderT m))

infixl 8 <?>
(<?>) :: App (Either e a) -> (e -> AppError) -> App a
m <?> f =
  m >>= \case
    Left err -> throwA (f err)
    Right x -> return x

(?>) :: App (Maybe a) -> AppError -> App a
m ?> err =
  m >>= \case
    Nothing -> throwA err
    Just x -> return x

displayAppError :: FilePath -> LazyByteString -> AppError -> String
displayAppError spec lbs = \case
  ReadSpecFileError _e -> "Couldn't open specification file: " <> spec
  ParserError e -> "Parse error: " <> e
  ScopeError tids -> uncurry (displayScopeError spec lbs) (head tids)
  InvalidDeploymentUrl url -> "Invalid deployment URL: " <> url
  HttpClientException op e -> displayHttpException op e
  HttpClientUnexpectedStatusCode _ _ -> "HTTP client returned 1xx or 3xx"
  HealthCheckFailed ->
    "Health check failed, make sure that the deployment is running."
  ResetFailed ->
    "Reset of the deploymnet failed, make sure that reset returns 2xx or exits with 0."

-- XXX: only displays one error at the time?!
displayScopeError ::
  FilePath -> LazyByteString -> Pos -> [Ann TypeId] -> String
displayScopeError fp lbs pos tids =
  let bs = LBS.toStrict lbs
      Ann pos' tid = head tids

      ls = linesUtf8 bs
      (l, _c, c') = case posLineCols bs [pos, pos'] of
        (l0, c0) : (_l1, c1) : _ -> (l0, c0, c1)
        _ -> error "displayScopeError: impossible"
      line = if l < length ls then ls !! l else ""
      linum = " " ++ show l
      lpad = map (const ' ') linum

      prevLine = if l - 1 >= 0 then ls !! (l - 1) else ""
  in  -- XXX: check $LANG for UTF8 support? otherwise use ascii box drawing?
      "Scope error, the type "
        ++ displayTypeId tid
        ++ " isn't defined.\n\n"
        ++ lpad
        ++ " ┌── "
        ++ fp
        ++ ":"
        ++ show l
        ++ ":"
        ++ show c'
        ++ "\n"
        ++ lpad
        ++ " │\n"
        ++ (if null prevLine then "" else lpad ++ " | " ++ prevLine ++ "\n")
        ++ linum
        ++ " │ "
        ++ line
        ++ "\n"
        ++
        -- XXX: We should avoid ANSI here, maybe introduce a "FancyError" type
        -- and let LibMain display it?
        lpad
        ++ " │ "
        ++ replicate c' ' '
        ++ Text.unpack (red (Text.replicate (length (displayTypeId tid)) "^"))
        ++ "\n\n"
        ++ "Either define the type or mark it as abstract, in case it shouldn't be\ngenerated."

displayHttpException :: Op -> HttpException -> String
displayHttpException op (HttpExceptionRequest _req content) = case content of
  ConnectionFailure _someException -> "Couldn't connect to host."
  StatusCodeException resp _ ->
    show (prettyOp op) <> "\n" <> show (Http.responseStatus resp)
  err -> show err
displayHttpException _op InvalidUrlException {} =
  error "impossible: already handled by InvalidDeploymentUrl"
