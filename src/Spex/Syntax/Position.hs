module Spex.Syntax.Position where

import qualified FlatParse.Basic as FP

------------------------------------------------------------------------

newtype Pos = Pos FP.Pos
  deriving (Eq, Ord, Show)

data Ann a = Ann
  { pos  :: Pos
  , item :: a
  }
  deriving (Eq, Ord, Show)
