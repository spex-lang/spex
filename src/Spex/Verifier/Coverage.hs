{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.Coverage where

import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter
import Prettyprinter.Render.String

import Spex.PrettyPrinter
import Spex.Syntax

------------------------------------------------------------------------

type HttpStatusCode = Int

newtype Coverage = Coverage
  {unCoverage :: Map HttpStatusCode (Map OpId Word)}
  deriving (Show, Semigroup, Monoid)

emptyCoverage :: Coverage
emptyCoverage = Coverage Map.empty

insertCoverage :: OpId -> HttpStatusCode -> Coverage -> Coverage
insertCoverage oid code (Coverage outerMap) =
  let
    innerMap = Map.findWithDefault Map.empty code outerMap
    innerMap' = Map.insertWith (+) oid 1 innerMap
  in
    Coverage $ Map.insert code innerMap' outerMap

filterOk :: Coverage -> [(OpId, Word)]
filterOk =
  concatMap snd
    . Map.toList
    . fmap Map.toList
    . Map.filterWithKey (\code _m -> 200 <= code && code < 300)
    . unCoverage

filter4xx :: Coverage -> [(HttpStatusCode, [(OpId, Word)])]
filter4xx = filterOnHttpStatusCode (\code -> 400 <= code && code < 500)

filter5xx :: Coverage -> [(HttpStatusCode, [(OpId, Word)])]
filter5xx = filterOnHttpStatusCode (\code -> 500 <= code && code < 600)

filterOnHttpStatusCode ::
  (HttpStatusCode -> Bool)
  -> Coverage
  -> [(HttpStatusCode, [(OpId, Word)])]
filterOnHttpStatusCode p =
  map (\xs -> (fst (head xs), concatMap snd xs))
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . Map.toList
    . fmap Map.toList
    . Map.filterWithKey (\code _m -> p code)
    . unCoverage

prettyCoverage :: Spec -> Coverage -> Doc x
prettyCoverage spec cov =
  vsep $
    [ "Coverage:"
    , indent
        2
        ( vcat
            [ "2xx:"
            , indent
                2
                ( vcat
                    (map prettyOpCov (filterOk cov))
                )
            , vcat
                (map prettyOpCovBad (filter4xx cov))
            , vcat
                (map prettyOpCovBad (filter5xx cov))
            ]
        )
    ]
      ++ ( if not (null notCoveredOps)
            then
              [ "Not covered (no non-404 responses):"
              , indent 2 (vcat (map prettyBS notCoveredOps))
              ]
            else []
         )
      ++ [ ""
         , "Total operations (ops): " <> pretty total
         ]
  where
    total :: Word
    total = getSum $ foldMap (foldMap Sum) (unCoverage cov)

    prettyOpCov :: (OpId, Word) -> Doc x
    prettyOpCov (oid, n) =
      pretty
        (round ((fromIntegral n / fromIntegral total :: Double) * 100) :: Int)
        <> "%"
        <+> prettyBS oid
        <+> "("
        <> pretty n
        <> " ops)"

    prettyOpCovBad :: (HttpStatusCode, [(OpId, Word)]) -> Doc x
    prettyOpCovBad (code, ops) =
      vsep [pretty code <> ":", indent 2 (vcat (map prettyOpCov ops))]

    coveredOps :: Set OpId
    coveredOps =
      Set.fromList
        . concatMap Map.keys
        . Map.elems
        -- Filter out 404s.
        . Map.filterWithKey (\code _m -> code /= 404)
        $ unCoverage cov

    declaredOps :: Set OpId
    declaredOps =
      Set.fromList (map (\op -> op.item.id) spec.component.opDecls)

    notCoveredOps :: [OpId]
    notCoveredOps = Set.toList $ declaredOps `Set.difference` coveredOps

displayCoverage :: Spec -> Coverage -> String
displayCoverage spec cov =
  renderString
    (layoutPretty defaultLayoutOptions (prettyCoverage spec cov))
