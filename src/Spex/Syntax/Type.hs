module Spex.Syntax.Type where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.String (IsString)

------------------------------------------------------------------------

data Type
  = UnitT
  | BoolT
  | StringT
  | IntT
  | UserT ByteString
  | RecordT (Record Type)
  -- | ArrayT (Vector Type)
  deriving Show

type Record a = Map Field a

newtype Field = Field ByteString
  deriving (Eq, Ord, Show, IsString)

data Method = Get | Post
  deriving Show

displayMethod :: Method -> String
displayMethod Get  = "GET"
displayMethod Post = "POST"
