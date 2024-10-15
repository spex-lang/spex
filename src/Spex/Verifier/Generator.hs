{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.Generator where

import Control.Monad
import Data.List (find)
import Data.Text (Text)
import Data.Vector (Vector)
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
generate spec prng =
  (runGen (genOp spec.component.opDecls
                 spec.component.typeDecls) prng', prng'')
  where
    (prng', prng'') = splitPrng prng

genOp :: [OpDecl] -> [TypeDecl] -> Gen Op
genOp opdecls ctx = do
  opdecl <- elements opdecls
  p <- genPath ctx opdecl.path
  b <- genBody ctx opdecl.body
  return $ Op
    { id           = opdecl.id
    , method       = opdecl.method
    , path         = p
    , body         = b
    , responseType = opdecl.responseType
    }

genValue :: [TypeDecl] -> Type -> Gen Value
genValue _ctx UnitT          = return UnitV
genValue _ctx BoolT          = BoolV <$> genBounded
genValue _ctx IntT           = IntV <$> genInt
genValue _ctx StringT        = StringV <$> genText
genValue  ctx (ArrayT tys)   = genArray ctx tys
genValue  ctx (RecordT ftys) = genRecord ctx ftys
genValue  ctx (UserT x)      = genUserDefined ctx x

genBody :: [TypeDecl] -> Maybe Type -> Gen (Maybe Value)
genBody ctx = traverse (genValue ctx)

genPath :: [TypeDecl] -> [PathSegment Type] -> Gen [PathSegment Value]
genPath ctx = traverse (traverse (genValue ctx))

genArray :: [TypeDecl] -> Vector Type -> Gen Value
genArray ctx = fmap ArrayV . traverse (genValue ctx)

genRecord :: [TypeDecl] -> Record Type -> Gen Value
genRecord ctx = fmap RecordV . traverse (genValue ctx)

genUserDefined :: [TypeDecl] -> TypeId -> Gen Value
genUserDefined ctx tid = case find (\tydecl -> tydecl.id == tid) ctx of
  Nothing -> error "genUserDefined: impossible, scope checker should have caught this"
  Just tydecl -> genValue ctx tydecl.rhs
