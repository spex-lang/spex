module Spex.Generator.Combinator (module Spex.Generator.Combinator) where

import Control.Monad
import Data.Maybe (fromJust, isJust)
import System.Random

import Spex.Generator.Prng

------------------------------------------------------------------------

type Size = Int

newtype Gen a = Gen (StdGen -> Size -> (a, StdGen))

instance Monad Gen where
  return = pure
  Gen f >>= k = Gen $ \stdGen size ->
    let (x, stdGen') = f stdGen size
        Gen g = k x
    in  g stdGen' size

instance Applicative Gen where
  pure x = Gen (\stdGen _size -> (x, stdGen))
  (<*>) = ap

instance Functor Gen where
  fmap = liftM

runGen :: Gen a -> Prng -> Size -> (Prng, a)
runGen (Gen k) (Prng stdGen) size = (Prng stdGen', x)
  where
    (x, stdGen') = k stdGen size

sized :: (Int -> Gen a) -> Gen a
sized k = Gen (\stdGen size -> let Gen f = k size in f stdGen size)

getSize :: Gen Size
getSize = sized pure

resize :: Size -> Gen a -> Gen a
resize size _ | size < 0 = error "resize: negative size"
resize size (Gen f) = Gen (\stdGen _size -> f stdGen size)

choose :: (Random a) => (a, a) -> Gen a
choose rng = Gen (\stdGen _ -> randomR rng stdGen)

-- NOTE: This can be optimised later (if needed), see QuickCheck.
chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose

elements :: [a] -> Gen a
elements [] = error "elements used with empty list"
elements xs = (xs !!) `fmap` chooseInt (0, length xs - 1)

oneof :: [Gen a] -> Gen a
oneof [] = error "oneof used with empty list"
oneof gs = chooseInt (0, length gs - 1) >>= (gs !!)

vectorOf :: Int -> Gen a -> Gen [a]
vectorOf = replicateM

listOf :: Gen a -> Gen [a]
listOf gen = sized $ \n -> do
  k <- choose (0, n)
  vectorOf k gen

chooseBounded :: (Random a, Bounded a) => Gen a
chooseBounded = choose (minBound, maxBound)

chooseFloat :: Gen Float
chooseFloat = choose (0, 1)

chooseDouble :: Gen Double
chooseDouble = choose (0, 1)

generate' :: Gen a -> IO a
generate' (Gen g) = do
  (Prng stdGen, _seed) <- newPrng Nothing
  let size = 30
  return (fst (g stdGen size))

sample' :: Gen a -> IO [a]
sample' g =
  generate' (sequence [resize n g | n <- [0, 2 .. 20]])

------------------------------------------------------------------------

suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p = do
  mx <- gen `suchThatMaybe` p
  case mx of
    Just x -> return x
    Nothing -> sized (\n -> resize (n + 1) (gen `suchThat` p))

suchThatMap :: Gen a -> (a -> Maybe b) -> Gen b
gen `suchThatMap` f =
  fmap fromJust $ fmap f gen `suchThat` isJust

suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
gen `suchThatMaybe` p = sized (\n -> try n (2 * n))
  where
    try m n
      | m > n = return Nothing
      | otherwise = do
          x <- resize m gen
          if p x then return (Just x) else try (m + 1) n
