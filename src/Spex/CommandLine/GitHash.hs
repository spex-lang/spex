{-# LANGUAGE TemplateHaskell #-}

module Spex.CommandLine.GitHash where

import Data.String (fromString)
import GitHash
import Language.Haskell.TH (Exp, Q, runIO)
import System.Environment (lookupEnv)

------------------------------------------------------------------------

tGitHash :: Q Exp
tGitHash = runIO $ do
  lookupEnv "SPEX_GIT_COMMIT" >>= \case
    Just v  -> [| fromString v |]
    Nothing -> getGitInfo "." >>= \case
                 Right gi -> [| fromString (giHash gi ++ if giDirty gi
                                                  then "-dirty"
                                                  else "") |]
                 Left _e  -> error "tGitHash: can't find git commit"
