{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.Reseter where

import Control.Exception
import Data.Text qualified as Text
import System.Exit
import System.Process

import Spex.Monad
import Spex.PrettyPrinter
import Spex.Syntax
import Spex.Verifier.HttpClient

------------------------------------------------------------------------

reseter :: HttpClient -> Reset -> App ()
reseter client reset = do
  case reset of
    ResetPath path -> do
      let path' = Text.dropWhile (== '/') path
      let op = Op "reset" noHeaders Delete [Path path'] Nothing UnitT
      debug (displayOp op)
      eResp <- tryApp $ httpRequest client op
      case eResp of
        Left _err -> throw ResetFailed
        Right Ok2xx {} -> return ()
        Right ClientError4xx {} -> throw ResetFailed
        Right ServerError5xx {} -> throw ResetFailed
    ResetScript fp -> do
      (exitCode, _out, _err) <- liftIO (readProcessWithExitCode fp [] "")
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure _code -> throw ResetFailed
