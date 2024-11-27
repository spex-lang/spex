{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier.Result (module Spex.Verifier.Result) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter
import Prettyprinter.Render.Text (putDoc, renderStrict)

import Spex.PrettyPrinter
import Spex.Syntax
import Spex.TypeChecker (TypeError)
import Spex.Verifier.Coverage

------------------------------------------------------------------------

data ResultJson = ResultJson
  { failingTests :: [FailingTest]
  , coverage :: Coverage
  , seed :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Result = Result
  { failingTests :: [FailingTest] -- XXX: Set?
  , coverage :: Coverage
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Semigroup Result where
  r1 <> r2 =
    Result
      { failingTests = r1.failingTests <> r2.failingTests
      , coverage = r1.coverage <> r2.coverage
      }

instance Monoid Result where
  mempty = Result mempty mempty
  mappend = (<>)

data FailingTest = FailingTest
  { test :: [Op]
  , failure :: Failure
  , body :: Text
  , shrinks :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Failure
  = DecodeFailure String
  | TypeErrorFailure TypeError
  | StatusCodeFailure HttpStatusCode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

putResult :: Spec -> Result -> Int -> IO ()
putResult spec res seed = putDoc (prettyResult spec res seed)

displayResult :: Spec -> Result -> Int -> Text
displayResult spec res seed =
  renderStrict $
    layoutPretty defaultLayoutOptions $
      prettyResult spec res seed

prettyResult :: Spec -> Result -> Int -> Doc x
prettyResult spec res seed =
  let lenInterestingTests = length res.failingTests
  in  indent 2 $
        vcat
          ( [ ""
            , "Found"
                <+> viaShow lenInterestingTests
                <+> "intereresting test case"
                <> ( if lenInterestingTests > 1
                      then "s"
                      else ""
                   )
                <> if lenInterestingTests == 0 then "." else ":" <> line
            ]
              ++ ( if null res.failingTests
                    then [""]
                    else
                      [ indent 2 $
                          vcat (zipWith prettyTest [1 ..] res.failingTests)
                      ]
                 )
              ++ [ prettyCoverage spec res.coverage
                 , ""
                 , "Use --seed " <> viaShow seed <> " to reproduce this run."
                 ]
          )

prettyTest :: Int -> FailingTest -> Doc x
prettyTest i t =
  viaShow i
    <> ". "
    <> align
      ( vcat
          ( map prettyOp t.test
              ++ indent 2 (prettyFailure t.failure <+> pretty t.body)
              : if t.shrinks > 0
                then
                  [ "("
                      <> viaShow t.shrinks
                      <> " shrink"
                      <> (if t.shrinks > 1 then "s" else "")
                      <> ")"
                  , ""
                  ]
                else [""]
          )
      )

prettyFailure :: Failure -> Doc x
prettyFailure (DecodeFailure err) = "↳ JSON decode failure: " <> pretty err
prettyFailure (TypeErrorFailure err) = "↳ JSON type error failure: " <> viaShow err
prettyFailure (StatusCodeFailure code) = "↳ " <> viaShow code
