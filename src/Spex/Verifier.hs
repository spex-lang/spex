{-# LANGUAGE OverloadedStrings #-}

module Spex.Verifier where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.List.NonEmpty qualified as NonEmpty
import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Render.Text (putDoc)

import Spex.CommandLine.Option
import Spex.Monad
import Spex.PrettyPrinter
import Spex.Syntax
import Spex.TypeChecker
import Spex.Verifier.Codec.Json
import Spex.Verifier.Coverage
import Spex.Verifier.Generator
import Spex.Verifier.Generator.Env
import Spex.Verifier.HttpClient
import Spex.Verifier.Reseter
import Spex.Verifier.Shrinker

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

------------------------------------------------------------------------

verify :: VerifyOptions -> Spec -> Deployment -> Prng -> App Result
verify opts spec deployment prng = do
  client <- newHttpClient deployment
  reseter client deployment.reset
  verifyLoop
    opts
    spec
    deployment
    client
    opts.numTests
    []
    prng
    emptyGenEnv
    mempty

verifyLoop ::
  VerifyOptions
  -> Spec
  -> Deployment
  -> HttpClient
  -> Word
  -> [Op]
  -> Prng
  -> GenEnv
  -> Result
  -> App Result
verifyLoop opts spec deployment client = go
  where
    go ::
      Word
      -> [Op]
      -> Prng
      -> GenEnv
      -> Result
      -> App Result
    go 0 _ops _prng _genEnv res = do
      debug_ ""
      return res
    go n ops prng genEnv res = do
      (op, prng', genEnv') <- generate spec prng genEnv
      debug (displayOp op)
      resp <- httpRequest client op
      debug_ $ "  ↳ " <> show resp.statusCode <> " " <> BS8.unpack resp.body
      let cov' = insertCoverage op.id resp.statusCode res.coverage
      let r = case resp of
            Ok2xx {} -> do
              case decode resp.body of
                Left err -> Left (DecodeFailure err)
                Right val -> case typeCheck' spec.component.typeDecls val op.responseType of
                  Nothing -> Right val
                  Just err -> Left (TypeErrorFailure err)
            _4xxOr5xx -> Left (StatusCodeFailure resp.statusCode)
      case r of
        Left failure0 -> do
          res' <-
            -- If we've seen a failure already, then don't add it to the test
            -- cases.
            if failure0 `elem` map failure res.failingTests
              then return (res {coverage = cov'})
              else do
                test <-
                  counterExample
                    (op : ops)
                    failure0
                    resp.body
                -- If the shrunk test case is a singleton operation and it's a
                -- 404, then don't count that as a failing test case.
                let failingTests'
                      | length test.test == 1 && test.failure == StatusCodeFailure 404 =
                          res.failingTests
                      | otherwise = test : res.failingTests
                return (Result failingTests' cov')
          go (n - 1) [] prng' emptyGenEnv res'
        Right val -> do
          let genEnv'' = insertValue op.responseType val genEnv'
          go (n - 1) (op : ops) prng' genEnv'' res {coverage = cov'}

    counterExample ::
      [Op]
      -> Failure
      -> ByteString
      -> App FailingTest
    counterExample ops failure body = do
      trace ("Shrinking:\n" <> displayOps ops)
      ops' <-
        if opts.noShrinking
          then return (NonEmpty.singleton ops)
          else
            shrinker
              (shrinkProp spec.component.typeDecls client deployment.reset)
              (shrinkList shrinkOp)
              (reverse ops)
      trace ("Shrunk to:\n" <> displayOps (NonEmpty.last ops'))
      return
        ( FailingTest
            (NonEmpty.last ops')
            failure
            body
            (NonEmpty.length ops' - 1)
        )

shrinkProp :: [TypeDecl] -> HttpClient -> Reset -> [Op] -> App Bool
shrinkProp ctx client reset ops0 = do
  reseter client reset
  go ops0
  where
    go [] = debug_ "" >> return True
    go (op : ops) = do
      debug (displayOp op)
      resp <- httpRequest client op
      case resp of
        Ok2xx code body -> do
          case decode body of
            Left _err -> return False
            Right val -> do
              debug_ $ "  ↳ " <> show code <> " " <> displayValue val
              let ok = typeCheck ctx val op.responseType
              if ok
                then go ops
                else return False
        ClientError4xx code _msg -> do
          debug_ $ "  ↳ " <> show code
          return False
        ServerError5xx {} -> return False

shrinkOp :: Op -> [Op]
shrinkOp _op = []
