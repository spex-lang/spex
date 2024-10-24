{-# LANGUAGE TemplateHaskell #-}

module Spex.CommandLine.GitHash where

import Data.String (fromString)
import Language.Haskell.TH (Exp, Q, runIO)

------------------------------------------------------------------------

tGitHash :: Q Exp
tGitHash = runIO $ do
  s <- readFile "SPEX_VERSION"
  [| fromString s |]
