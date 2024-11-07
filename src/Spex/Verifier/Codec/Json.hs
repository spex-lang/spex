module Spex.Verifier.Codec.Json where

import Data.Aeson qualified as Json
import Data.Aeson.Key qualified as Json
import Data.Aeson.KeyMap qualified as Json
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Map qualified as Map
import Data.Text.Encoding qualified as Text

import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------

decode :: ByteString -> Either String Value
decode bs = case Json.eitherDecodeStrict bs of
  Left err -> Left err
  Right json -> Right (fromJson json)

fromJson :: Json.Value -> Value
fromJson json = case json of
  Json.Null -> UnitV
  Json.Bool b -> BoolV b
  Json.Number i -> IntV (round i) -- XXX: BAD
  Json.String txt -> StringV txt
  Json.Array js -> ArrayV (fmap fromJson js)
  Json.Object obj -> RecordV (fmap fromJson (Map.mapKeys keyToField (Json.toMap obj)))

encode :: Value -> LazyByteString
encode = Json.encode . toJson

toJson :: Value -> Json.Value
toJson UnitV = Json.Null
toJson (BoolV b) = Json.Bool b
toJson (IntV i) = Json.Number (fromIntegral i)
toJson (StringV txt) = Json.String txt
toJson (ArrayV vs) = Json.Array (fmap toJson vs)
toJson (RecordV fvs) = Json.Object (Json.fromMap (Map.mapKeys fieldToKey (fmap toJson fvs)))

fieldToKey :: Field -> Json.Key
fieldToKey (Field bs) = Json.fromText (Text.decodeUtf8Lenient bs)

keyToField :: Json.Key -> Field
keyToField = Field . Text.encodeUtf8 . Json.toText
