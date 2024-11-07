module Spex.Syntax.Value where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)

import Spex.Syntax.Type (Record, displayArray, displayRecord)

------------------------------------------------------------------------

data Value
  = UnitV
  | BoolV Bool
  | IntV Int
  | StringV Text
  | ArrayV (Vector Value)
  | RecordV (Record Value)
  deriving (Eq, Ord, Show)

displayValue :: Value -> String
displayValue UnitV = "()"
displayValue (BoolV b) = show b
displayValue (IntV i) = show i
displayValue (StringV t) = Text.unpack t
displayValue (ArrayV vs) = displayArray displayValue vs
displayValue (RecordV fvs) = displayRecord displayValue " = " fvs
