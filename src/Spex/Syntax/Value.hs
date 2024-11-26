{-# LANGUAGE DeriveAnyClass #-}

module Spex.Syntax.Value where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Spex.Syntax.Type (Record)

------------------------------------------------------------------------

data Value
  = UnitV
  | BoolV Bool
  | IntV Int
  | StringV Text
  | ArrayV (Vector Value)
  | RecordV (Record Value)
  deriving (Eq, Ord, Show, Generic, ToJSON)
