{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.Generator where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Foldable (toList)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Vector (Vector)
import System.Random

import qualified Spex.Monad
import Spex.Monad (App, GenEnv, debug, trace)
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

instance Monad Gen where
  return      = pure
  Gen f >>= k = Gen $ \prng ->
    let
      (prng', prng'') = split prng
      Gen g = k (f prng')
    in
      g prng''

instance Applicative Gen where
  pure x = Gen (\_prng -> x)
  (<*>)  = ap

instance Functor Gen where
  fmap = liftM

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
-- genInt = choose (minBound, maxBound)
genInt = choose (-1000, 1000)

genText :: Gen Text
genText = elements ["foo", "bar", "qux"]

------------------------------------------------------------------------

type GenCtx = [TypeDecl]

data GenMEnv = GenMEnv
  { genCtx :: GenCtx
  , genEnv :: GenEnv
  }

type GenM a = ReaderT GenMEnv Gen a

runGenM :: GenM a -> GenCtx -> GenEnv -> Prng -> a
runGenM m ctx env prng = runGen (runReaderT m (GenMEnv ctx env)) prng

generate :: Spec -> Prng -> App (Op, Prng, GenEnv)
generate spec prng = do
  env <- Spex.Monad.asks Spex.Monad.genEnv -- XXX:
  let ctx             = spec.component.typeDecls
      (prng', prng'') = splitPrng prng
      (decl, op)      = runGenM (genOp ctx env spec.component.opDecls) ctx env prng'
  let env' = newValues ctx env decl op
  trace $ "generate, new values: " <> show env'
  return  (op, prng'', env')

newValues :: GenCtx -> GenEnv -> OpDecl -> Op -> GenEnv
newValues ctx old decl op =
  let
    tys  = concatMap removeRecordType $ map (removeUserDefined ctx . removeMode) (toList decl)
    vals = concatMap removeRecord $ toList op
  in
    foldl' (\ih (ty, val) -> Map.insertWith (<>) (Right ty) (Set.singleton val) ih) old (zip tys vals)

removeUserDefined :: GenCtx -> Type -> Type
removeUserDefined ctx (UserT tid) =
  case find (\tydecl -> tydecl.id == tid) ctx of
    Nothing   -> error "removeUserDefined: impossible, scope checker"
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

genOp :: GenCtx -> GenEnv -> [OpDecl] -> GenM (OpDecl, Op)
genOp ctx env opdecls = do
  opdecl <- lift (elements opdecls)
  p <- genPath opdecl.path
  b <- genBody opdecl.body
  return (opdecl, Op
    { id           = opdecl.id
    , method       = opdecl.method
    , path         = p
    , body         = b
    , responseType = opdecl.responseType
    })

remembering :: Type -> Gen Value -> Mode -> GenM Value
remembering ty g mode = do
  ctx <- asks genCtx
  env <- asks genEnv
  case mode of
    Abstract   -> case Map.lookup (Right ty) env of
                         Nothing -> lift g
                         Just vs -> lift (elements (Set.toList vs))
    _otherwise -> lift g

data Mode = Normal | Abstract | Unique

genValue :: Type -> GenM Value
genValue UnitT          = return UnitV
genValue BoolT          = BoolV <$> lift genBounded
genValue IntT           = remembering IntT (IntV <$> genInt) Normal
genValue StringT        = StringV <$> lift genText
genValue (ArrayT tys)   = genArray tys
genValue (RecordT ftys) = genRecord Normal ftys
genValue (UserT x)      = genUserDefined Normal x
genValue (AbstractT ty) = genValue' ty Abstract
genValue (UniqueT ty)   = genValue' ty Unique

genValue' :: Type -> Mode -> GenM Value
genValue' UnitT          _mode = return UnitV
genValue' BoolT          _mode = BoolV <$> lift genBounded
genValue' IntT            mode = remembering IntT (IntV <$> genInt) mode
genValue' StringT        _mode = StringV <$> lift genText
genValue' (ArrayT tys)   _mode = genArray tys
genValue' (RecordT ftys)  mode = genRecord mode ftys
genValue' (UserT x)       mode = genUserDefined mode x
genValue' (AbstractT ty) _mode = error ""
genValue' (UniqueT ty)   _mode = error ""

genBody :: Maybe Type -> GenM (Maybe Value)
genBody = traverse genValue

genPath :: [PathSegment Type] -> GenM [PathSegment Value]
genPath = traverse (traverse genValue)

genArray :: Vector Type -> GenM Value
genArray = fmap ArrayV . traverse genValue

genRecord :: Mode -> Record Type -> GenM Value
genRecord mode = fmap RecordV . traverse (flip genValue' mode)

genUserDefined :: Mode -> TypeId -> GenM Value
genUserDefined mode tid = do
  ctx <- asks genCtx
  case find (\tydecl -> tydecl.id == tid) ctx of
    Nothing     -> error "genUserDefined: impossible, scope checker"
    Just tydecl -> genValue' tydecl.rhs mode
