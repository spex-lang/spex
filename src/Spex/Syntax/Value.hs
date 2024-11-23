module Spex.Syntax.Value where

import Data.Text (Text)
import Data.Vector (Vector)

import Spex.Syntax.Type (Record)

------------------------------------------------------------------------

data Value
  = UnitV
  | BoolV Bool
  | IntV Int
  | StringV Text
  | ArrayV (Vector Value)
  | RecordV (Record Value)
  deriving (Eq, Ord, Show)
