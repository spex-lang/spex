module Spex.Verifier where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Spex.Monad
import Spex.Syntax
import Spex.Syntax.Operation
import Spex.Syntax.Value
import Spex.TypeChecker
import Spex.Verifier.Codec.Json
import Spex.Verifier.Generator
import Spex.Verifier.Generator.Env
import Spex.Verifier.HttpClient
import Spex.Verifier.Reseter

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

verify :: Spec -> Deployment -> App Result
verify spec deployment = do
  numTests <- asks numTests
  mSeed <- asks mSeed
  (prng, seed) <- liftIO (newPrng mSeed)
  client <- newHttpClient deployment
  reseter client deployment.reset
  go numTests [] seed prng client 0 Map.empty
  where
    go :: Word -> [Op] -> Int -> Prng -> HttpClient -> Word -> Coverage -> App Result
    go 0 _ops _seed _prng _client n4xx cov = do
      debug_ ""
      return (Result [] n4xx cov)
    go n  ops  seed  prng  client n4xx cov = do
      (op, prng', env') <- generate spec prng
      let cov' = Map.insertWith (+) op.id 1 cov
      debug (displayOp displayValue op)
      resp <- httpRequest client op
      case resp of
        Ok2xx body -> do
          val <- pure (decode body) <?> HttpClientDecodeError op body
          debug_ $ "  ↳ 2xx " <> displayValue val
          let ok = typeCheck spec.component.typeDecls val op.responseType
          if ok
          then local (\e -> e { genEnv = insertValue op.responseType val env' }) $
                 go (n - 1) (op : ops) seed prng' client n4xx cov'
          else do
            info $ "Typechecking failed, val: " ++ show val ++ " not of type " ++ show op.responseType
            let shrink xs _reset = xs
            let ops' = shrink (reverse (op : ops)) deployment.reset
            throwA (TestFailure (show ops') seed)
        ClientError4xx code -> do
          debug_ $ "  ↳ " <> show code
          if code == 404
          then local (\e -> e { genEnv = env' }) $
                 go (n - 1) ops seed prng' client (n4xx + 1) cov'
          else do
            let shrink xs _reset = xs
            let ops' = shrink (reverse (op : ops)) deployment.reset
            throwA (TestFailure (show ops') seed)
        ServerError5xx _code -> do
          let shrink xs _reset = xs
          let ops' = shrink (reverse (op : ops)) deployment.reset
          throwA (TestFailure (show ops') seed)
