{-# LANGUAGE OverloadedStrings #-}

module Spex.Logger (module Spex.Logger) where

import Data.Text (Text)

import Spex.CommandLine.Ansi

------------------------------------------------------------------------

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
