module Spex.Verifier.Result (module Spex.Verifier.Result) where

import Data.ByteString (ByteString)
import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Render.Text (putDoc)

import Spex.PrettyPrinter
import Spex.Syntax
import Spex.TypeChecker (TypeError)
import Spex.Verifier.Coverage

------------------------------------------------------------------------

data Result = Result
  { failingTests :: [FailingTest] -- XXX: Set?
  , coverage :: Coverage
  }
  deriving (Show)

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
  , body :: ByteString
  , shrinks :: Int
  }
  deriving (Show)

data Failure
  = DecodeFailure String
  | TypeErrorFailure TypeError
  | StatusCodeFailure HttpStatusCode
  deriving (Show, Eq)

putResult :: Spec -> Result -> Int -> IO ()
putResult spec res seed = putDoc (prettyResult spec res seed)

displayResult :: Spec -> Result -> Int -> String
displayResult spec res seed =
  renderString $
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
              ++ indent 2 (prettyFailure t.failure <+> prettyBS t.body)
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
