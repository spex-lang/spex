{-# LANGUAGE TemplateHaskell #-}

module Spex.CommandLine.GitHash where

import Control.Exception
import Data.String (fromString)
import GitHash
import Language.Haskell.TH (Exp, Q, runIO)

------------------------------------------------------------------------

tGitHash :: Q Exp
tGitHash = runIO $ do
  v <- readFile "/run/secrets/version" `catch`
         (\(_ :: IOError) ->
           getGitInfo "." >>= \case
             Right gi -> return (giHash gi ++ if giDirty gi then "-dirty" else "")
             Left _e  -> error "tGitHash: can't find git commit")
  [| fromString v |]
