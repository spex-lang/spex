module Spex.Verifier.Generator.Env where

import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Spex.Syntax.Type
import Spex.Syntax.Value

------------------------------------------------------------------------

newtype GenEnv = GenEnv (Map Type (Set Value))
  deriving (Show)

emptyGenEnv :: GenEnv
emptyGenEnv = GenEnv Map.empty

insertValue :: Type -> Value -> GenEnv -> GenEnv
insertValue ty val (GenEnv m) =
  GenEnv (Map.insertWith (<>) ty (Set.singleton val) m)

insertValues :: [(Type, Value)] -> GenEnv -> GenEnv
insertValues tyVals (GenEnv m) =
  GenEnv $
    foldl'
      (\ih (ty, val) -> Map.insertWith (<>) ty (Set.singleton val) ih)
      m
      tyVals

lookupValue :: Type -> GenEnv -> Maybe (Set Value)
lookupValue ty (GenEnv m) = Map.lookup ty m
