{-# LANGUAGE OverloadedStrings #-}

module Spex.Logger (module Spex.Logger) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock
import Data.Time.Format.ISO8601

import Spex.CommandLine.Ansi

------------------------------------------------------------------------

data LoggerKind = PlainLogger | JsonLogger

data InfoKind = PlainInfo | NormalInfo | DoneInfo

data DebugKind = PlainDebug | NormalDebug

data Logger = Logger
  { loggerKind :: LoggerKind
  , loggerInfo :: InfoKind -> Text -> IO ()
  , loggerError :: Text -> IO ()
  , loggerDebug :: DebugKind -> Text -> IO ()
  , loggerTrace :: Text -> IO ()
  , loggerFlush :: IO ()
  , loggerClose :: IO ()
  }

-- XXX: Check for unicode support, for checkmark?
noAnsiLogger :: (Text -> IO ()) -> IO () -> IO () -> Logger
noAnsiLogger printer flusher closer =
  Logger
    { loggerKind = PlainLogger
    , loggerInfo = \k -> case k of
        PlainInfo -> printer
        NormalInfo -> printer . ("i " <>)
        DoneInfo -> printer . ("✓ " <>)
    , loggerError = printer . ("Error: " <>)
    , loggerDebug = \_k _msg -> return ()
    , loggerTrace = \_msg -> return ()
    , loggerFlush = flusher
    , loggerClose = closer
    }

ansiLogger :: (Text -> IO ()) -> IO () -> IO () -> Logger
ansiLogger printer flusher closer =
  Logger
    { loggerKind = PlainLogger
    , loggerInfo = \k -> case k of
        PlainInfo -> printer
        NormalInfo -> printer . (cyan "i " <>)
        DoneInfo -> printer . (green "✓ " <>)
    , loggerError = printer . ((boldRed "Error" <> ": ") <>)
    , loggerDebug = \_k _msg -> return ()
    , loggerTrace = \_msg -> return ()
    , loggerFlush = flusher
    , loggerClose = closer
    }

jsonLogger :: (Text -> IO ()) -> IO () -> IO () -> Logger
jsonLogger printer flusher closer =
  Logger
    { loggerKind = JsonLogger
    , loggerInfo = \_k -> logJson printer "info"
    , loggerError = logJson printer "error"
    , loggerDebug = \_k _msg -> return ()
    , loggerTrace = \_msg -> return ()
    , loggerFlush = flusher
    , loggerClose = closer
    }

logJson :: (Text -> IO ()) -> Text -> Text -> IO ()
logJson printer level msg = do
  if Text.null (Text.strip msg)
    then return ()
    else do
      t <- getCurrentTime
      printer $
        Text.concat
          [ "{\"timestamp\":\""
          , Text.pack (iso8601Show t)
          , "\","
          , "\"level\":\""
          , level
          , "\","
          , "\"message\":\""
          , Text.strip msg
          , "\"}"
          ]

quietLogger :: Logger -> Logger
quietLogger l = l {loggerInfo = \_ _ -> return ()}

verboseLogger :: (Text -> IO ()) -> Logger -> Logger
verboseLogger printer l = case l.loggerKind of
  PlainLogger ->
    l
      { loggerDebug = \k -> case k of
          PlainDebug -> printer
          NormalDebug -> printer . (faint "d " <>)
      }
  JsonLogger -> l {loggerDebug = \_k -> logJson printer "debug"}

traceLogger :: (Text -> IO ()) -> Logger -> Logger
traceLogger printer l = case l.loggerKind of
  PlainLogger ->
    (verboseLogger printer l)
      { loggerTrace = printer . (faint "t " <>)
      }
  JsonLogger ->
    (verboseLogger printer l)
      { loggerTrace = logJson printer "trace"
      }
