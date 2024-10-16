module Spex.TypeChecker where

import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Spex.Syntax
import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------

typeCheck :: [TypeDecl] -> Value -> Type -> Bool
typeCheck _ctx UnitV         UnitT          = True
typeCheck _ctx BoolV {}      BoolT          = True
typeCheck _ctx IntV {}       IntT           = True
typeCheck _ctx StringV {}    StringT        = True
typeCheck _ctx (ArrayV vs)   UnitT          = Vector.null vs
typeCheck  ctx (ArrayV vs)   (ArrayT tys)   = Vector.and (Vector.zipWith (typeCheck ctx) vs tys)
typeCheck  ctx (RecordV fvs) (RecordT ftys) =
  Map.keys fvs == Map.keys ftys &&
  and (zipWith (typeCheck ctx) (Map.elems fvs) (Map.elems ftys))
typeCheck ctx (RecordV fvs) (UserT tid) =
  case find (\tyDecl -> tyDecl.id == tid) ctx of
    Nothing -> error "typeCheck: impossible, due to scopechecker"
    Just tyDecl -> typeCheck ctx (RecordV fvs) tyDecl.rhs
typeCheck _ _ _ = False
