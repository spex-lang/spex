{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.HealthChecker where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit
import System.Process

import Spex.Monad
import Spex.Syntax
import Spex.Verifier.HttpClient

------------------------------------------------------------------------

-- | Retries for 50 * 0.2s = 10s.
healthChecker :: Deployment -> App ()
healthChecker deployment@(Deployment _hostPort health _reset) = do
  case health of
    HealthCheckPath path -> do
      client <- newHttpClient deployment
      http 50 (Text.dropWhile (== '/') path) client
    HealthCheckScript fp -> script 50 fp
  where
    script :: Word -> FilePath -> App ()
    script 0 _fp = info_ "" >> throw HealthCheckFailed
    script n fp = do
      (exitCode, _out, _err) <- liftIO (readProcessWithExitCode fp [] "")
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure _code -> do
          wait n
          script (n - 1) fp

    http :: Word -> Text -> HttpClient -> App ()
    http 0 _path _client = info_ "" >> throw HealthCheckFailed
    http n path client = do
      eResp <-
        tryApp $
          httpRequest
            client
            (Op "health" (HttpParameter noHeaders Get [Path path] Nothing) UnitT)
      case eResp of
        Left _err -> do
          wait n
          http (n - 1) path client
        Right Ok2xx {} -> return ()
        Right ClientError4xx {} -> do
          wait n
          http (n - 1) path client
        Right ServerError5xx {} -> do
          wait n
          http (n - 1) path client

    wait n = do
      liftIO (threadDelay 200000) -- 0.2s
      when (n `mod` 10 == 0) $ do
        info_ "."
