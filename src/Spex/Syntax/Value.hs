module Spex.Syntax.Value where

import Data.Text (Text)

import Spex.Syntax.Type (Record)

------------------------------------------------------------------------

data Value
  = UnitV
  | BoolV Bool
  | IntV Int
  | StringV Text
  | RecordV (Record Value)
  deriving Show
