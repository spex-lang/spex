{-# LANGUAGE OverloadedStrings #-}

module Spex.Mock (module Spex.Mock) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Read (decimal)
import Network.HTTP.Types.Method (
  methodDelete,
  methodGet,
  methodPost,
  methodPut,
 )
import Network.HTTP.Types.Status (notFound404, ok200)
import Network.Wai
import Network.Wai.Handler.Warp

import Spex.CommandLine.Option (MockOptions (..))
import Spex.Syntax
import Spex.Verifier.Codec.Json
import Spex.Verifier.Generator
import Spex.Verifier.Generator.Env

------------------------------------------------------------------------

runMock :: MockOptions -> Spec -> IO ()
runMock opts spec = do
  (prng, seed) <- liftIO (newPrng opts.seed)
  state <- newIORef (MockState {prng = prng, genEnv = emptyGenEnv})
  run opts.port (waiApp spec state)

data MockState = MockState
  { prng :: Prng
  , genEnv :: GenEnv
  }

waiApp :: Spec -> IORef MockState -> Application
waiApp spec state req respond = do
  let ctx = spec.component.typeDecls
  let opDecls = map item (spec.component.opDecls)
  res <- atomicModifyIORef' state $ \s ->
    let (prng', prng'') = splitPrng s.prng
    in  case matchOp opDecls req of
          Nothing -> (s, Left notFound404)
          Just respTy ->
            ( s {prng = prng''}
            , Right (runGenM (genValue respTy) ctx s.genEnv prng')
            )
  case res of
    Left err -> respond $ responseLBS err [] ""
    Right val -> respond $ responseLBS ok200 [] (encode val)

-- XXX: check body?
matchOp :: [OpDecl] -> Request -> Maybe Type
matchOp ctx req =
  let ops' = filter (\op -> op.method `methodsMatch` requestMethod req) ctx
      ops'' = filter (\op -> op.path `pathMatch` pathInfo req) ops'
  in  case ops'' of
        [] -> Nothing
        [op] -> Just (op.responseType)
        _ ->
          --  XXX: we should check for this when checking the spec...
          error
            "matchOp: there can't be several ops with the same method and path"

pathMatch :: [PathSegment Type] -> [Text] -> Bool
pathMatch = go
  where
    go [] [] = True
    go (Path bs : ps) (t : ts) = bs == Text.encodeUtf8 t && go ps ts
    go (Hole _var ty : ps) (t : ts) = case ty of
      StringT -> go ps ts
      IntT -> case decimal t of
        Right _ -> go ps ts
        Left _ -> False
      _ty -> error ("pathMatch: unexpected type in path: " ++ show ty)
    go _ _ = False

methodsMatch :: Method -> ByteString -> Bool
methodsMatch Get = (== methodGet)
methodsMatch Put = (== methodPut)
methodsMatch Post = (== methodPost)
methodsMatch Delete = (== methodDelete)
