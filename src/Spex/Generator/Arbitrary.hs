module Spex.Generator.Arbitrary (module Spex.Generator.Arbitrary) where

import Data.Text (Text)
import Data.Text qualified as Text

import Spex.Generator.Combinator

------------------------------------------------------------------------

genInt :: Gen Int
genInt = arbitrarySizedIntegral

arbitrarySizedIntegral :: (Integral a) => Gen a
arbitrarySizedIntegral
  | isNonNegativeType fromI = arbitrarySizedNatural
  | otherwise = sized $ \n -> inBounds fromI (chooseInt (-n, n))
  where
    -- NOTE: this is a trick to make sure we get around lack of scoped type
    -- variables by pinning the result-type of fromIntegral.
    fromI = fromIntegral

isNonNegativeType :: (Enum a) => (Int -> a) -> Bool
isNonNegativeType fromI =
  case enumFromThen (fromI 1) (fromI 0) of
    [_, _] -> True
    _ -> False

arbitrarySizedNatural :: (Integral a) => Gen a
arbitrarySizedNatural =
  sized $ \n ->
    inBounds fromIntegral (chooseInt (0, n))

inBounds :: (Integral a) => (Int -> a) -> Gen Int -> Gen a
inBounds fi g = fmap fi (g `suchThat` (\x -> toInteger x == toInteger (fi x)))

genText :: Gen Text
genText = elements (map Text.pack ["foo", "bar", "qux"])
