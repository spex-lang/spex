{-# LANGUAGE OverloadedStrings #-}

module Spex.Mock (module Spex.Mock) where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Text (Text)
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
import Spex.Generator.Combinator
import Spex.Generator.Prng
import Spex.Syntax
import Spex.Verifier.Codec.Json
import Spex.Verifier.Generator
import Spex.Verifier.Generator.Env

------------------------------------------------------------------------

runMock :: MockOptions -> Spec -> Prng -> IO ()
runMock opts spec prng = do
  state <-
    newIORef (MockState {prng = prng, size = 0, genEnv = emptyGenEnv})
  run
    opts.port
    (addResetEndpoint state prng (addHealthEndpoint (waiApp spec state)))

data MockState = MockState
  { prng :: Prng
  , size :: Size
  , genEnv :: GenEnv
  }

addHealthEndpoint :: Middleware
addHealthEndpoint baseApp req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", ["health"]) -> respond (responseLBS ok200 [] "OK")
    _ -> baseApp req respond

addResetEndpoint :: IORef MockState -> Prng -> Middleware
addResetEndpoint state origPrng baseApp req respond =
  case (requestMethod req, pathInfo req) of
    ("DELETE", ["_reset"]) -> do
      atomicWriteIORef
        state
        (MockState {prng = origPrng, size = 0, genEnv = emptyGenEnv})
      respond (responseLBS ok200 [] "OK")
    _ -> baseApp req respond

waiApp :: Spec -> IORef MockState -> Application
waiApp spec ref req respond = do
  let ctx = spec.component.typeDecls
  let opDecls = map item (spec.component.opDecls)
  res <- atomicModifyIORef' ref $ \state ->
    case matchOp opDecls req of
      Nothing -> (state, Left notFound404)
      Just respTy ->
        let (prng', val) = runGenM (genValue respTy) ctx state.genEnv state.prng state.size
        in  ( state {prng = prng', genEnv = insertValue respTy val state.genEnv}
            , Right val
            )
  case res of
    Left err -> respond $ responseLBS err [] ""
    Right val -> respond $ responseLBS ok200 [] (encode val)

-- XXX: check body and put its fields into the genEnv?
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
    go (Path p : ps) (t : ts) = p == t && go ps ts
    go (Hole _var ty : ps) (t : ts) = case ty of
      StringT -> go ps ts
      IntT -> case parseInt t of
        Right (i, _rest) -> go ps ts -- XXX: put i into genEnv?
        Left _ -> False
      AbstractT IntT -> case parseInt t of
        Right (i, _rest) -> go ps ts
        Left _ -> False
      AbstractT StringT -> go ps ts
      _ty -> error ("pathMatch: unexpected type in path: " ++ show ty)
    go _ _ = False

    parseInt :: Text -> Either String (Int, Text)
    parseInt = decimal

methodsMatch :: Method -> ByteString -> Bool
methodsMatch Get = (== methodGet)
methodsMatch Put = (== methodPut)
methodsMatch Post = (== methodPost)
methodsMatch Delete = (== methodDelete)
