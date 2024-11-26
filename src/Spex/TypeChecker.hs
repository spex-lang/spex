module Spex.TypeChecker where

import Control.Exception (throw)
import Data.Foldable (toList)
import Data.List (find)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector

import Spex.Monad
import Spex.Syntax

------------------------------------------------------------------------

scopeCheck :: Spec -> App ()
scopeCheck spec
  | freeTypes_ `Set.isSubsetOf` userDefined = return ()
  | otherwise =
      throw . ScopeError . map (fmap Set.toList) $
        -- Set.filter (\ty -> ty.item `Set.member` missing) freeTypes
        filter
          (\(pos, tys) -> any (\ty -> ty.item `Set.member` missing) tys)
          freeTypesPos
  where
    missing :: Set TypeId
    missing = Set.difference freeTypes_ userDefined

    freeTypes_ :: Set TypeId
    freeTypes_ = Set.map item freeTypes

    freeTypes :: Set (Ann TypeId)
    freeTypes =
      foldMap
        userDefinedTypes
        (concatMap (toList . item) spec.component.opDecls)

    freeTypesPos :: [(Pos, Set (Ann TypeId))]
    freeTypesPos =
      map
        (\opDecl -> (opDecl.pos, foldMap userDefinedTypes (toList opDecl.item)))
        spec.component.opDecls

    userDefined :: Set TypeId
    userDefined = foldMap (Set.singleton . typeId) spec.component.typeDecls

data TypeError = ValueDoesntHaveType Value Type
  deriving (Show, Eq)

typeCheck' :: [TypeDecl] -> Value -> Type -> Maybe TypeError
typeCheck' ctx val ty
  | typeCheck ctx val ty = Nothing
  | otherwise = Just (ValueDoesntHaveType val ty)

typeCheck :: [TypeDecl] -> Value -> Type -> Bool
typeCheck _ctx UnitV UnitT = True
typeCheck _ctx BoolV {} BoolT = True
typeCheck _ctx IntV {} IntT = True
typeCheck _ctx StringV {} StringT = True
typeCheck _ctx (ArrayV vs) UnitT = Vector.null vs
typeCheck ctx (ArrayV vs) (ArrayT tys) = Vector.and (Vector.zipWith (typeCheck ctx) vs tys)
typeCheck ctx (RecordV fvs) (RecordT ftys) =
  Map.keys fvs == Map.keys ftys
    && and (zipWith (typeCheck ctx) (Map.elems fvs) (Map.elems ftys))
typeCheck ctx (RecordV fvs) (UserT tid) =
  case find (\tyDecl -> tyDecl.typeId == tid.item) ctx of
    Nothing -> error "typeCheck: impossible, due to scopechecker"
    Just tyDecl -> typeCheck ctx (RecordV fvs) tyDecl.rhs
typeCheck _ _ _ = False
