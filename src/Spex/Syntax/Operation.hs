module Spex.Syntax.Operation where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.List (foldl', intersperse)
import Data.String (IsString)

import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------
--
data OpF a = Op
  { id :: OpId
  , method :: Method
  , path :: [PathSegment a]
  , body :: Maybe a
  , responseType :: Type
  }
  deriving (Show, Functor, Foldable, Traversable)

newtype OpId = OpId ByteString
  deriving (Eq, Ord, Show, IsString)

type Op = OpF Value

type OpDecl = OpF Type

data PathSegment a = Path ByteString | Hole ByteString a
  deriving (Show, Functor, Foldable, Traversable)
