module Spex.Syntax.Type where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Vector (Vector)

import Spex.Syntax.Position

------------------------------------------------------------------------

data Type
  = UnitT
  | BoolT
  | StringT
  | IntT
  | ArrayT (Vector Type)
  | RecordT (Record Type)
  | UserT (Ann TypeId)
  | AbstractT Type
  | UniqueT Type
  deriving (Eq, Ord, Show)

newtype TypeId = TypeId ByteString
  deriving (Eq, Ord, Show, IsString)

type Record a = Map Field a

newtype Field = Field ByteString
  deriving (Eq, Ord, Show, IsString)

userDefinedTypes :: Type -> Set (Ann TypeId)
userDefinedTypes = \case
  UnitT -> mempty
  BoolT -> mempty
  StringT -> mempty
  IntT -> mempty
  ArrayT tys -> foldMap userDefinedTypes tys
  RecordT ftys -> foldMap userDefinedTypes ftys
  UserT tid -> Set.singleton tid
  AbstractT ty -> mempty -- NOTE: Abstract types don't need to be in scope.
  UniqueT ty -> userDefinedTypes ty

data Method = Get | Post | Put | Delete
  deriving (Show)

displayTypeId :: TypeId -> String
displayTypeId (TypeId bs) = BS8.unpack bs
