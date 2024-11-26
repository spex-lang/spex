{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Spex.Syntax.Type where

import Data.Aeson (ToJSON, ToJSONKey)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

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
  deriving (Eq, Ord, Show, Generic, ToJSON)

newtype TypeId = TypeId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, ToJSON, ToJSONKey)

type Record a = Map Field a

newtype Field = Field Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, ToJSONKey, IsString)

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
  deriving (Show, Generic, ToJSON)
