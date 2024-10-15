module Spex.Syntax.Operation where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.String (IsString)

import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------
--
data OpF a = Op
  { id           :: OpId
  , method       :: Method
  , path         :: [PathSegment a]
  , query        :: Maybe String
  , body         :: Maybe a
  , responseType :: Type
  }
  deriving Show

newtype OpId = OpId ByteString
  deriving (Show, IsString)

displayOpId :: OpId -> String
displayOpId (OpId bs) = BS8.unpack bs

type Op = OpF Value

type OpDecl = OpF Type

data PathSegment a = Path ByteString | Hole ByteString a
  deriving Show

displayOp :: OpF a -> String
displayOp op = displayOpId op.id <> " : " <> displayMethod op.method -- <> displayPath op.path <> displayQuery op.query <> displayBody op.body <> " -> " <> displayType
