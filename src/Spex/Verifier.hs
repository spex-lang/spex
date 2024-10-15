module Spex.Verifier where

import Spex.Monad
import Spex.Syntax
import Spex.Syntax.Operation
import Spex.TypeChecker
import Spex.Verifier.Generator
import Spex.Verifier.HttpClient

------------------------------------------------------------------------

data Config = Config
  { _numTests :: Word
  , _seed     :: Maybe Int
  }

verify :: Config -> Spec -> Deployment -> App ()
verify (Config numTests mSeed) spec deployment = do
  (prng, seed) <- liftIO (newPrng mSeed)
  client <- newHttpClient deployment
  go numTests [] seed prng client
  where
    go :: Word -> [Op] -> Int -> Prng -> HttpClient -> App ()
    go 0 _ops _seed _prng _client = return ()
    go n  ops  seed  prng  client = do
      let (op, prng') = generate spec prng
      resp <- httpRequest client op
      let ok = typeCheck resp op.responseType
      if ok
      then go (n - 1) (op : ops) seed prng' client
      else do
        let shrink xs _reset = xs
        let ops' = shrink (reverse (op : ops)) deployment.reset
        throwA (TestFailure (show ops') seed)
