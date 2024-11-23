module Spex.Verifier.Generator where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Foldable (toList)
import Data.List (find)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)

import Spex.Generator.Arbitrary
import Spex.Generator.Combinator
import Spex.Generator.Prng
import Spex.Monad
import Spex.Syntax
import Spex.Verifier.Generator.Env

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
