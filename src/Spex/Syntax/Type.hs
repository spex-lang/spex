module Spex.Syntax.Type where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
  | AbstractT Type
  | UniqueT Type
  deriving (Eq, Ord, Show)

newtype TypeId = TypeId ByteString
  deriving (Eq, Ord, Show, IsString)

type Record a = Map Field a

newtype Field = Field ByteString
  deriving (Eq, Ord, Show, IsString)

userDefinedTypes :: Type -> Set TypeId
userDefinedTypes = \case
  UnitT        -> mempty
  BoolT        -> mempty
  StringT      -> mempty
  IntT         -> mempty
  ArrayT tys   -> foldMap userDefinedTypes tys
  RecordT ftys -> foldMap userDefinedTypes ftys
  UserT tid    -> Set.singleton tid
  AbstractT ty -> userDefinedTypes ty
  UniqueT ty   -> userDefinedTypes ty

data Method = Get | Post | Put | Delete
  deriving Show

displayMethod :: Method -> String
displayMethod Get    = "GET"
displayMethod Post   = "POST"
displayMethod Put    = "PUT"
displayMethod Delete = "DELETE"

displayType :: Type -> String
displayType UnitT          = "Unit"
displayType BoolT          = "Bool"
displayType StringT        = "String"
displayType IntT           = "Int"
displayType (ArrayT a)     = displayArray displayType a
displayType (RecordT ftys) = displayRecord displayType " : " ftys
displayType (UserT tid)    = displayTypeId tid
displayType (AbstractT ty) = "@" <> displayType ty
displayType (UniqueT ty)   = "!" <> displayType ty

displayTypeId :: TypeId -> String
displayTypeId (TypeId bs) = BS8.unpack bs

displayArray :: (a -> String) -> Vector a -> String
displayArray d xs =
  "[" <> concat (intersperse ", " (Vector.toList (fmap d xs))) <> "]"

displayRecord :: (a -> String) -> String -> Record a -> String
displayRecord d sep r =
  "{" <> concat (intersperse ", " (map go (Map.toList r))) <> "}"
  where
    go (f, x) = displayField f <> sep <> d x

displayField :: Field -> String
displayField (Field bs) = BS8.unpack bs
