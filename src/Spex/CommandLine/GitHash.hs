{-# LANGUAGE TemplateHaskell #-}

module Spex.CommandLine.GitHash where

import Control.Exception
import Data.String (fromString)
import GitHash
import Language.Haskell.TH (Exp, Q, runIO)

------------------------------------------------------------------------

tGitHash :: Q Exp
tGitHash = runIO $ do
  s <- readFile "SPEX_VERSION" `catch` ((\(_e :: IOError) -> do
    getGitInfo "." >>= \case
      Left _e -> error "tGitHash: can't find git commit"
      Right gi -> return (giHash gi ++ if giDirty gi then "-dirty" else "")))
  [| fromString s |]
