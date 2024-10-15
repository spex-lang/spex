{-# LANGUAGE OverloadedStrings #-}

module Spex.CommandLine.Spinner where

import Control.Concurrent
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Conc.Sync (ThreadStatus(..), threadStatus)
import System.Console.ANSI
import System.Exit
import System.IO
import System.Posix.Signals

------------------------------------------------------------------------

data Spinner = Spinner
  { threadId :: ThreadId
  , stop     :: MVar ()
  , lock     :: MVar ()
  , nls      :: IORef Int
  }

lineFrames :: Text
lineFrames = "-\\|/"

newSpinner :: Text -> Text -> Int -> IO Spinner
newSpinner msg frames delayMicros = do
  hHideCursor stdout
  var <- newEmptyMVar
  lock <- newMVar ()
  nls <- newIORef 0
  Just (row, col) <- getCursorPosition
  hSaveCursor stdout
  tid <- forkIO (go 0 row col var lock nls)
  _ <- installHandler keyboardSignal (Catch (killThread tid >> exitWith (ExitFailure 130))) Nothing
  return (Spinner tid var lock nls)
  where
    go i row col var lock nls = do
      b <- isEmptyMVar var
      withMVar lock $ \_ -> do
        n <- readIORef nls
        -- print ("go: ", row, col)
        let frame = Text.index frames (i `mod` Text.length frames)
        hSetCursorPosition stdout (row - n) col
        hPutStr stdout (replicate (2 + Text.length msg) '\b')
        if b
        then hPutChar stdout frame
        else do
          hSetSGR stdout [SetColor Foreground Vivid Green]
          hPutChar stdout '✓'
          hSetSGR stdout [Reset]
        Text.hPutStr stdout (" " <> msg)
      threadDelay delayMicros
      if b
      then go (i + 1) row col var lock nls
      else do
        hShowCursor stdout
        putStrLn ""

stopSpinner :: Spinner -> IO ()
stopSpinner s = do
  putMVar s.stop ()
  waitUntilDead s.threadId
    where
      waitUntilDead tid = do
        status <- threadStatus tid
        case status of
          ThreadFinished -> return ()
          ThreadDied -> return ()
          _otherwise -> threadDelay 10000 >> waitUntilDead tid

------------------------------------------------------------------------

testSpinner :: IO ()
testSpinner = do
  s <- newSpinner "Waiting for something..." lineFrames 100000
  threadDelay 2000000
  withMVar s.lock $ \_ -> do
    hRestoreCursor stdout
    Just (row, _col) <- getCursorPosition
    hSetCursorPosition stdout (row + 1) 0
    hPutStrLn stdout "\n\nprogress 1"
    modifyIORef s.nls (+ 3)
  threadDelay 2000000
  withMVar s.lock $ \_ -> do
    hRestoreCursor stdout
    Just (row, _col) <- getCursorPosition
    hSetCursorPosition stdout (row + 3) 0
    hPutStrLn stdout "\nprogress 2"
    modifyIORef s.nls (+ 2)
  threadDelay 2000000
  stopSpinner s
  hRestoreCursor stdout
  Just (row, _col) <- getCursorPosition
  hSetCursorPosition stdout (row + 3) 0
  hPutStrLn stdout "\nFinished!"
