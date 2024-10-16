module Spex.TypeChecker where

import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------

typeCheck :: Value -> Type -> Bool
typeCheck UnitV         UnitT          = True
typeCheck BoolV {}      BoolT          = True
typeCheck IntV {}       IntT           = True
typeCheck StringV {}    StringT        = True
typeCheck (ArrayV vs)   UnitT          = Vector.null vs
typeCheck (ArrayV vs)   (ArrayT tys)   = Vector.and (Vector.zipWith typeCheck vs tys)
typeCheck (RecordV fvs) (RecordT ftys) =
  Map.keys fvs == Map.keys ftys &&
  and (zipWith typeCheck (Map.elems fvs) (Map.elems ftys))
typeCheck _             _              = False
