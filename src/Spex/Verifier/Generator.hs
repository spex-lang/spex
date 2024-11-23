{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.Generator where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Foldable (toList)
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector (Vector)
import System.Random

import Spex.Monad
import Spex.Syntax
import Spex.Verifier.Generator.Env

------------------------------------------------------------------------

newtype Prng = Prng StdGen

newPrng :: Maybe Int -> IO (Prng, Int)
newPrng Nothing = do
  seed <- randomIO
  return (Prng (mkStdGen seed), seed)
newPrng (Just seed) = return (Prng (mkStdGen seed), seed)

mkPrng :: Int -> Prng
mkPrng seed = Prng (mkStdGen seed)

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
genText = elements ["foo", "bar", "qux"]

------------------------------------------------------------------------

type GenCtx = [TypeDecl]

data GenMEnv = GenMEnv
  { genMCtx :: GenCtx
  , genMEnv :: GenEnv
  }

type GenM a = ReaderT GenMEnv Gen a

runGenM :: GenM a -> GenCtx -> GenEnv -> Prng -> Size -> (Prng, a)
runGenM m ctx env prng size =
  runGen (Reader.runReaderT m (GenMEnv ctx env)) prng size

generate :: Spec -> Prng -> Size -> GenEnv -> App (Op, Prng, GenEnv)
generate spec prng size env = do
  let ctx = spec.component.typeDecls
      (prng', (decl, op)) =
        runGenM (genOp (map item spec.component.opDecls)) ctx env prng size
      env' = newValues ctx env decl op
  trace $ "generate, new values: " <> show env'
  return (op, prng', env')

newValues :: GenCtx -> GenEnv -> OpDecl -> Op -> GenEnv
newValues ctx old decl op =
  let
    tys =
      concatMap removeRecordType $
        map (removeUserDefined ctx . removeMode) (toList decl)
    vals = concatMap removeRecord $ toList op
  in
    insertValues (zip tys vals) old

removeUserDefined :: GenCtx -> Type -> Type
removeUserDefined ctx (UserT tid) =
  case find (\tydecl -> tydecl.typeId == tid.item) ctx of
    Nothing -> error "removeUserDefined: impossible, due to scope checker"
    Just decl -> decl.rhs
removeUserDefined _ctx ty = ty

removeMode :: Type -> Type
removeMode (AbstractT ty) = ty
removeMode (UniqueT ty) = ty
removeMode ty = ty

removeRecord :: Value -> [Value]
removeRecord (RecordV fvs) = concatMap removeRecord (Map.elems fvs)
removeRecord v = [v]

removeRecordType :: Type -> [Type]
removeRecordType (RecordT ftys) = concatMap removeRecordType (Map.elems ftys)
removeRecordType ty = [ty]

genOp :: [OpDecl] -> GenM (OpDecl, Op)
genOp opdecls = do
  opdecl <- lift (elements opdecls)
  hs <- genHeaders opdecl.headers
  p <- genPath opdecl.path
  b <- genBody opdecl.body
  return
    ( opdecl
    , Op
        { id = opdecl.id
        , headers = hs
        , method = opdecl.method
        , path = p
        , body = b
        , responseType = opdecl.responseType
        }
    )

remembering :: Type -> Gen Value -> Mode -> GenM Value
remembering ty g mode = do
  env <- Reader.asks genMEnv
  case mode of
    Abstract -> case lookupValue ty env of
      Nothing -> lift g
      Just vs -> lift (elements (Set.toList vs))
    _otherwise -> lift g

data Mode = Normal | Abstract | Unique

genValue :: Type -> GenM Value
genValue UnitT = return UnitV
genValue BoolT = BoolV <$> lift chooseBounded
genValue IntT = remembering IntT (IntV <$> genInt) Normal
genValue StringT = StringV <$> lift genText
genValue (ArrayT tys) = genArray tys
genValue (RecordT ftys) = genRecord Normal ftys
genValue (UserT x) = genUserDefined Normal x.item
genValue (AbstractT ty) = genValue' ty Abstract
genValue (UniqueT ty) = genValue' ty Unique

genValue' :: Type -> Mode -> GenM Value
genValue' UnitT _mode = return UnitV
genValue' BoolT _mode = BoolV <$> lift chooseBounded
genValue' IntT mode = remembering IntT (IntV <$> genInt) mode
genValue' StringT _mode = StringV <$> lift genText
genValue' (ArrayT tys) _mode = genArray tys
genValue' (RecordT ftys) mode = genRecord mode ftys
genValue' (UserT x) mode = genUserDefined mode x.item
genValue' (AbstractT ty) _mode = error ""
genValue' (UniqueT ty) _mode = error ""

genBody :: Maybe Type -> GenM (Maybe Value)
genBody = traverse genValue

genHeaders :: [HeaderF Type] -> GenM [HeaderF Value]
genHeaders = traverse (traverse genValue)

genPath :: [PathSegment Type] -> GenM [PathSegment Value]
genPath = traverse (traverse genValue)

genArray :: Vector Type -> GenM Value
genArray = fmap ArrayV . traverse genValue

genRecord :: Mode -> Record Type -> GenM Value
genRecord mode = fmap RecordV . traverse (flip genValue' mode)

genUserDefined :: Mode -> TypeId -> GenM Value
genUserDefined mode tid = do
  ctx <- Reader.asks genMCtx
  case find (\tydecl -> tydecl.typeId == tid) ctx of
    Nothing -> error "genUserDefined: impossible, scope checker"
    Just tydecl -> genValue' tydecl.rhs mode
