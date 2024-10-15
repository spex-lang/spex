module Spex.Verifier.Codec.Json where

import qualified Data.Aeson as Json
import Data.ByteString (ByteString)

import Spex.Syntax.Value

------------------------------------------------------------------------

decode :: ByteString -> Either String Value
decode bs = case Json.eitherDecodeStrict bs of
  Left  err  -> Left err
  Right json -> Right (fromJson json)

fromJson :: Json.Value -> Value
fromJson json = case json of
  -- Object !Object
  -- Array !Array
  Json.String txt -> StringV txt
  -- Number !Scientific
  Json.Bool b -> BoolV b
  Json.Null   -> UnitV
  _ -> error "fromJson: not implemented yet"
