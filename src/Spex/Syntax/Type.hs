module Spex.Syntax.Type where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

------------------------------------------------------------------------

data Type
  = UnitT
  | BoolT
  | StringT
  | IntT
  | ArrayT (Vector Type)
  | RecordT (Record Type)
  | UserT TypeId
  deriving Show

newtype TypeId = TypeId ByteString
  deriving (Eq, Ord, Show, IsString)

type Record a = Map Field a

newtype Field = Field ByteString
  deriving (Eq, Ord, Show, IsString)

data Method = Get | Post
  deriving Show

displayMethod :: Method -> String
displayMethod Get  = "GET"
displayMethod Post = "POST"

displayType :: Type -> String
displayType UnitT       = "Unit"
displayType BoolT       = "Bool"
displayType StringT     = "String"
displayType IntT        = "Int"
displayType (ArrayT a)  = displayArray displayType a
displayType (RecordT r) = displayRecord displayType r
displayType (UserT tid) = displayTypeId tid

displayTypeId :: TypeId -> String
displayTypeId (TypeId bs) = BS8.unpack bs

displayArray :: (a -> String) -> Vector a -> String
displayArray d xs =
  "[" <> concat (intersperse ", " (Vector.toList (fmap d xs))) <> "]"

displayRecord :: (a -> String) -> Record a -> String
displayRecord d r =
  "{" <> concat (intersperse ", " (map go (Map.toList r))) <> "}"
  where
    go (f, x) = displayField f <> " : " <> d x

displayField :: Field -> String
displayField (Field bs) = BS8.unpack bs
