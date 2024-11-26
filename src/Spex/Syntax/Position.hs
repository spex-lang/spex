{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Spex.Syntax.Position where

import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import FlatParse.Basic qualified as FP
import GHC.Generics (Generic)

------------------------------------------------------------------------

newtype Pos = Pos Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON)

data Ann a = Ann
  { pos :: Pos
  , item :: a
  }
  deriving stock
    (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToJSON)

linesUtf8 :: ByteString -> [String]
linesUtf8 = FP.linesUtf8

posLineCols :: ByteString -> [Pos] -> [(Int, Int)]
posLineCols bs poss = FP.posLineCols bs (coerce poss)
