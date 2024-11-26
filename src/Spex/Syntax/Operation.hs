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
--
data OpF a = Op
  { id :: OpId
  , headers :: [HeaderF a]
  , method :: Method
  , path :: [PathSegment a]
  , body :: Maybe a
  , responseType :: Type
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToJSON)

newtype OpId = OpId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, ToJSON, ToJSONKey)

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

type Header = HeaderF Value
type HeaderDecl = HeaderF Type

noHeaders :: [HeaderF a]
noHeaders = []

type Op = OpF Value

type OpDecl = OpF Type

data PathSegment a = Path Text | Hole Text a
  deriving (Show, Functor, Foldable, Traversable, Generic, ToJSON)
