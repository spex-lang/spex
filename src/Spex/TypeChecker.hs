module Spex.TypeChecker where

import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------

typeCheck :: Value -> Type -> Bool
typeCheck StringV {} StringT = True
typeCheck IntV {}    IntT    = True
typeCheck BoolV {}   BoolT   = True
typeCheck UnitV      UnitT   = True
typeCheck _          _       = False
