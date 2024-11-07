module Spex.Syntax.Position where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import FlatParse.Basic qualified as FP

------------------------------------------------------------------------

newtype Pos = Pos Int
  deriving (Eq, Ord, Show)

data Ann a = Ann
  { pos :: Pos
  , item :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

linesUtf8 :: ByteString -> [String]
linesUtf8 = FP.linesUtf8

posLineCols :: ByteString -> [Pos] -> [(Int, Int)]
posLineCols bs poss = FP.posLineCols bs (coerce poss)
