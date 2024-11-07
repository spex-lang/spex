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

displayOpId :: OpId -> String
displayOpId (OpId bs) = BS8.unpack bs

type Op = OpF Value

type OpDecl = OpF Type

data PathSegment a = Path ByteString | Hole ByteString a
  deriving (Show, Functor, Foldable, Traversable)

displayOp :: (a -> String) -> OpF a -> String
displayOp d op =
  concat $
    [ displayOpId op.id
    , " : "
    , displayMethod op.method
    , " "
    , "/"
    , displayPath d op.path
    , maybe "" (" " <>) (fmap d op.body)
    ]
      ++ if op.responseType == UnitT
        then []
        else [" -> ", displayType op.responseType]

displayPath :: (a -> String) -> [PathSegment a] -> String
displayPath d = concat . intersperse "/" . map (displayPathSegment . fmap d)

displayPathSegment :: PathSegment String -> String
displayPathSegment (Path p) = BS8.unpack p
displayPathSegment (Hole _x s) = s

displayOps :: [Op] -> String
displayOps ops =
  foldl'
    ( \ih (i, op) -> show i <> ". " <> displayOp displayValue op <> "\n" <> ih
    )
    ""
    . zip [length ops, length ops - 1 ..]
    $ ops
