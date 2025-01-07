{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Spex.Syntax.Operation where

import Data.Aeson (ToJSON, ToJSONKey)
import Data.Aeson qualified as JSON
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)

import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------

data OpF parameter a = Op
  { id :: OpId
  , parameter :: parameter
  , responseType :: Type
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToJSON)

newtype OpId = OpId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, ToJSON, ToJSONKey)

------------------------------------------------------------------------

type Op = OpF (HttpParameter Value) Value

type OpDecl = OpF (HttpParameter Type) Type

data HttpParameter a = HttpParameter
  { headers :: [HeaderF a]
  , method :: Method
  , path :: [PathSegment a]
  , body :: Maybe a
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToJSON)

type Header = HeaderF Value
type HeaderDecl = HeaderF Type

noHeaders :: [HeaderF a]
noHeaders = []

data HeaderF a = Header ByteString (Maybe a)
  deriving (Show, Functor, Foldable, Traversable)

instance (ToJSON a) => ToJSON (HeaderF a) where
  toJSON (Header header Nothing) =
    JSON.String
      (Text.decodeUtf8Lenient header)
  toJSON (Header header (Just value)) = case JSON.toJSON value of
    JSON.String value_ ->
      JSON.String
        (Text.decodeUtf8Lenient header <> value_)
    _otherwise -> error "toJSON, HeaderF: impossible"

data PathSegment a = Path Text | Hole Text a
  deriving (Show, Functor, Foldable, Traversable, Generic, ToJSON)
