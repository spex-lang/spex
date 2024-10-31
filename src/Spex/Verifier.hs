module Spex.Verifier where

import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (ByteString)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Spex.CommandLine.Option
import Spex.Monad
import Spex.Syntax
import Spex.TypeChecker
import Spex.Verifier.Codec.Json
import Spex.Verifier.Generator
import Spex.Verifier.Generator.Env
import Spex.Verifier.HttpClient
import Spex.Verifier.Reseter
import Spex.Verifier.Shrinker

------------------------------------------------------------------------

data Result = Result
  { failingTests :: [FailingTest] -- XXX: Set?
  , clientErrors :: Word
  , coverage     :: Coverage
  }
  deriving Show

type Coverage = Map OpId Word

data FailingTest = FailingTest
  { test :: [Op]
  , code :: Int
  , body :: ByteString
  , seed :: Int
  }
  deriving Show

displayResult :: Result -> String
displayResult res = unlines
  [ "  failing tests: " <> show res.failingTests
  , "  client errors: " <> show res.clientErrors
  , "  coverage:      " <> show res.coverage
  ]

------------------------------------------------------------------------

verify :: VerifyOptions -> Spec -> Deployment -> App Result
verify opts spec deployment = do
  (prng, seed) <- liftIO (newPrng opts.seed)
  client <- newHttpClient deployment
  reseter client deployment.reset
  go opts.numTests [] seed prng emptyGenEnv client 0 Map.empty
  where
    go :: Word -> [Op] -> Int -> Prng -> GenEnv -> HttpClient -> Word -> Coverage -> App Result
    go 0 _ops _seed _prng _genEnv _client n4xx cov = do
      debug_ ""
      return (Result [] n4xx cov)
    go n  ops  seed  prng  genEnv client n4xx cov = do
      (op, prng', genEnv') <- generate spec prng genEnv
      let cov' = Map.insertWith (+) op.id 1 cov
      debug (displayOp displayValue op)
      resp <- httpRequest client op
      case resp of
        Ok2xx body -> do
          val <- pure (decode body) <?> HttpClientDecodeError op body
          debug_ $ "  ↳ 2xx " <> displayValue val
          let ok = typeCheck spec.component.typeDecls val op.responseType
          if ok
          then do
            let genEnv'' = insertValue op.responseType val genEnv'
            go (n - 1) (op : ops) seed prng' genEnv'' client n4xx cov'
          else do
            let err = "Typechecking failed, val: " ++ show val ++ " not of type " ++ show op.responseType
            counterExample client (op : ops) err seed
        ClientError4xx code msg -> do
          let ret = "  ↳ " <> show code
          debug_ ret
          if code == 404
          then go (n - 1) ops seed prng' genEnv' client (n4xx + 1) cov'
          else counterExample client (op : ops) (ret <> " " <> BS8.unpack msg) seed
        ServerError5xx code msg ->
          counterExample client (op : ops) ("  ↳ " <> show code <> " " <> BS8.unpack msg) seed

    counterExample :: HttpClient -> [Op] -> String -> Int -> App Result
    counterExample client ops err seed = do
      ops' <- if opts.noShrinking
              then return (NonEmpty.singleton ops)
              else shrinker
                     (shrinkProp spec.component.typeDecls client deployment.reset)
                     (shrinkList shrinkOp) (reverse ops)
      throwA (TestFailure (NonEmpty.last ops') (NonEmpty.length ops' - 1) err seed)

shrinkProp :: [TypeDecl] -> HttpClient -> Reset -> [Op] -> App Bool
shrinkProp ctx client reset ops0 = do
  reseter client reset
  go ops0
  where
    go []         = debug_ "" >> return True
    go (op : ops) = do
      debug (displayOp displayValue op)
      resp <- httpRequest client op
      case resp of
        Ok2xx body -> do
          val <- pure (decode body) <?> HttpClientDecodeError op body
          debug_ $ "  ↳ 2xx " <> displayValue val
          let ok = typeCheck ctx val op.responseType
          if ok
          then go ops
          else return False
        ClientError4xx code _msg -> do
          debug_ $ "  ↳ " <> show code
          if code == 404
          then go ops
          else return False
        ServerError5xx {} -> return False

shrinkOp :: Op -> [Op]
shrinkOp _op = []
