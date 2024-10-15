module Spex.Verifier where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Spex.Monad
import Spex.Syntax
import Spex.Syntax.Operation
import Spex.TypeChecker
import Spex.Verifier.Codec.Json
import Spex.Verifier.Generator
import Spex.Verifier.HttpClient

------------------------------------------------------------------------

data Result = Result
  { failingTests :: [FailingTest] -- XXX: Set?
  , clientErrors :: Word
  , coverage     :: Map OpId Word
  }
  deriving Show

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
  go numTests [] seed prng client 0
  where
    go :: Word -> [Op] -> Int -> Prng -> HttpClient -> Word -> App Result
    go 0 _ops _seed _prng _client n4xx = return (Result [] n4xx Map.empty)
    go n  ops  seed  prng  client n4xx = do
      let (op, prng') = generate spec prng
      resp <- httpRequest client op
      case resp of
        Ok2xx body -> do
          val <- pure (decode body) <?> HttpClientDecodeError op body
          let ok = typeCheck val op.responseType
          if ok
          then go (n - 1) (op : ops) seed prng' client n4xx
          else do
            let shrink xs _reset = xs
            let ops' = shrink (reverse (op : ops)) deployment.reset
            throwA (TestFailure (show ops') seed)
        ClientError4xx -> go (n - 1) ops seed prng' client (n4xx + 1)
        ServerError5xx -> do
          let shrink xs _reset = xs
          let ops' = shrink (reverse (op : ops)) deployment.reset
          throwA (TestFailure (show ops') seed)
