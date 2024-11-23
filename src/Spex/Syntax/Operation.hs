module Spex.Syntax.Operation where

import Data.ByteString (ByteString)
import Data.String (IsString)

import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------
--
data OpF a = Op
  { id :: OpId
  , headers :: [HeaderF a]
  , method :: Method
  , path :: [PathSegment a]
  , body :: Maybe a
  , responseType :: Type
  }
  deriving (Show, Functor, Foldable, Traversable)

newtype OpId = OpId ByteString
  deriving (Eq, Ord, Show, IsString)

data HeaderF a = Header ByteString (Maybe a)
  deriving (Show, Functor, Foldable, Traversable)

type Header = HeaderF Value
type HeaderDecl = HeaderF Type

noHeaders :: [HeaderF a]
noHeaders = []

type Op = OpF Value

type OpDecl = OpF Type

data PathSegment a = Path ByteString | Hole ByteString a
  deriving (Show, Functor, Foldable, Traversable)
