{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.Generator where

import Control.Monad
import Data.Text (Text)
import System.Random

import Spex.Syntax
import Spex.Syntax.Operation
import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------

newtype Prng = Prng StdGen

newPrng :: Maybe Int -> IO (Prng, Int)
newPrng Nothing = do
  seed <- randomIO
  return (Prng (mkStdGen seed), seed)
newPrng (Just seed) = return (Prng (mkStdGen seed), seed)

splitPrng :: Prng -> (Prng, Prng)
splitPrng (Prng g) = (Prng g', Prng g'')
  where
    (g', g'') = split g

------------------------------------------------------------------------

newtype Gen a = Gen (StdGen -> a)
  deriving (Functor, Applicative, Monad)

runGen :: Gen a -> Prng -> a
runGen (Gen k) (Prng g) = k g

rand :: Gen StdGen
rand = Gen (\g -> g)

choose :: Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) `fmap` rand

elements :: [a] -> Gen a
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

vector :: Gen a -> Int -> Gen [a]
vector g n = replicateM n g

genBounded :: (Random a, Bounded a) => Gen a
genBounded = choose (minBound, maxBound)

genInt :: Gen Int
genInt = choose (minBound, maxBound)

genText :: Gen Text
genText = elements ["foo", "bar", "qux"]

generate :: Spec -> Prng -> (Op, Prng)
generate spec prng = (runGen (genOp spec.component.opDecls) prng', prng'')
  where
    (prng', prng'') = splitPrng prng

genOp :: [OpDecl] -> Gen Op
genOp decls = do
  decl <- elements decls
  p <- genPath decl.path
  b <- genBody decl.body
  return $ Op
    { id           = decl.id
    , method       = decl.method
    , path         = p
    , query        = Nothing
    , body         = b
    , responseType = decl.responseType
    }

genValue :: Type -> Gen Value
genValue UnitT        = return UnitV
genValue BoolT        = BoolV <$> genBounded
genValue IntT         = IntV <$> genInt
genValue StringT      = StringV <$> genText
genValue (RecordT fs) = undefined
genValue (UserT x)    = undefined

genBody :: Maybe Type -> Gen (Maybe Value)
genBody = traverse genValue

genPath :: [PathSegment Type] -> Gen [PathSegment Value]
genPath [] = return []
genPath (Path p : ps) = do
  xs <- genPath ps
  return (Path p : xs)
genPath (Hole x ty : ps) = do
  v <- genValue ty
  xs <- genPath ps
  return (Hole x v : xs)
