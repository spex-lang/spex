{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.Reseter where

import qualified Data.ByteString.Char8 as BS8
import System.Exit
import System.Process

import Spex.Monad
import Spex.Syntax
import Spex.Verifier.HttpClient

------------------------------------------------------------------------

reseter :: HttpClient -> Reset -> App ()
reseter client reset = do
  case reset of
    ResetPath path -> do
      let path' = BS8.dropWhile (== '/') path
      let op = Op "reset" Delete [Path path'] Nothing UnitT
      debug (displayOp displayValue op)
      eResp <- tryA $ httpRequest client op
      case eResp of
        Left _err -> throwA ResetFailed
        Right (Ok2xx _body) -> return ()
        Right ClientError4xx {} -> throwA ResetFailed
        Right ServerError5xx {} -> throwA ResetFailed

    ResetScript fp -> do
      (exitCode, _out, _err) <- liftIO (readProcessWithExitCode fp [] "")
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure _code -> throwA ResetFailed