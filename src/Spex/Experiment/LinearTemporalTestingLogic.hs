{-# LANGUAGE OverloadedRecordDot #-}

module Spex.Experiment.LinearTemporalTestingLogic (module Spex.Experiment.LinearTemporalTestingLogic) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe

import Spex.Experiment.Histogram

------------------------------------------------------------------------

infixr 1 :==>
infixr 3 `And`
infix 4 :==
infix 4 :<=
infixl 6 :+
infixl 6 :-
infixl 9 :.

data Form a
  = TT
  | FF
  | Prop (a -> Bool)
  | Neg (Form a)
  | X (Form a)
  | U (Form a) (Form a)
  | Always (Form a)
  | Eventually (Form a)
  | Or (Form a) (Form a)
  | And (Form a) (Form a)
  | (:==>) (Form a) (Form a)
  | Term :<= Term
  | Term :== Term
  | FreezeQuantifier Var (Form a)
  | PercentileLessThan Name Percentile Term Int

type Name = String
type Percentile = Double

data Term
  = Var Var
  | Int Int
  | String String
  | (:-) Term Term
  | (:+) Term Term
  | (:.) Term Field

type Var = String

data Field = Kind | Id | Time

data Message = Message {kind :: String, id :: Int, time :: Int}
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------

-- [](request -> <>(response /\ request.id == response.id /\ response.time - request.time <= 100ms)
example :: Form Message
example =
  Always $
    FreezeQuantifier "request" $
      Prop (\msg -> msg.kind == "request")
        :==> Eventually
          ( FreezeQuantifier "response" $
              Prop (\msg -> msg.kind == "response")
                `And` ( Var "response"
                          :. Id
                          :== Var "request"
                          :. Id
                      )
                `And` ( ( Var "response"
                            :. Time
                            :- Var "request"
                            :. Time
                        )
                          :<= Int 100
                      )
          )

------------------------------------------------------------------------

newtype Env a = Env
  { variables :: Map Var a
  }

emptyEnv :: Env a
emptyEnv = Env Map.empty

sat :: Form Message -> [Message] -> Env Message -> Bool
sat TT _w _env = True
sat FF _w _env = False
sat (Prop p) w _env = p (w !! 0)
sat (Neg p) w env = not (sat p w env)
sat (X p) w env = sat p (drop 1 w) env
sat (U p q) w env =
  case listToMaybe [i | i <- [0 .. length w - 1], sat q (drop i w) env] of
    Nothing -> False
    Just i -> and [sat p (drop k w) env | k <- [0 .. i - 1]]
sat (Always p) w env = and [sat p (drop i w) env | i <- [0 .. length w - 1]]
sat (Eventually p) w env = or [sat p (drop i w) env | i <- [0 .. length w - 1]]
sat (Or p q) w env = sat p w env || sat q w env
sat (And p q) w env = sat p w env && sat q w env
sat (p :==> q) w env = if sat p w env then sat q w env else True
sat (t1 :<= t2) _w env = eval t1 env <= eval t2 env
sat (t1 :== t2) _w env = eval t1 env == eval t2 env
sat (FreezeQuantifier x p) w env =
  sat p w env {variables = Map.insert x (w !! 0) env.variables}
sat (PercentileLessThan name percentile t j) _w env = case eval t env of
  IntValue i -> undefined -- Map.update name (measure i)
  _otherwise -> error "percentile less than of non-integer"

eval :: Term -> Env Message -> Value
eval (Var x) env = MessageValue (env.variables Map.! x)
eval (Int i) _env = IntValue i
eval (String s) _env = StringValue s
eval (t1 :+ t2) env = case (eval t1 env, eval t2 env) of
  (IntValue i1, IntValue i2) -> IntValue (i1 + i2)
  _otherwise -> error "addition of non-integers"
eval (t1 :- t2) env = case (eval t1 env, eval t2 env) of
  (IntValue i1, IntValue i2) -> IntValue (i1 - i2)
  _otherwise -> error "subtraction of non-integers"
eval (t :. field) env = case eval t env of
  MessageValue msg -> case field of
    Kind -> StringValue msg.kind
    Id -> IntValue msg.id
    Time -> IntValue msg.time
  _otherwise -> error "field access on non-message"

data Value = MessageValue Message | StringValue String | IntValue Int
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------

test :: Bool
test =
  and
    [ sat example [Message "request" 0 0, Message "response" 0 100] emptyEnv
    , not
        ( sat example [Message "request" 0 0, Message "response" 1 100] emptyEnv
        )
    , not
        ( sat example [Message "request" 0 0, Message "response" 0 101] emptyEnv
        )
    , sat example [Message "_equest" 0 0, Message "response" 0 100] emptyEnv
    , not
        ( sat example [Message "request" 0 0, Message "_esponse" 0 100] emptyEnv
        )
    , sat
        example
        [Message "foo" 0 0, Message "request" 0 0, Message "response" 0 100]
        emptyEnv
    ]

------------------------------------------------------------------------

newtype Histograms = Histograms
  { histograms :: Map String Histogram
  }

emptyHistograms :: [String] -> Histograms
emptyHistograms histogramNames =
  Histograms
    { histograms =
        Map.fromList
          (zip histogramNames (replicate (length histogramNames) newHistogram))
    }

sat' ::
  Form Message -> [Message] -> Env Message -> State Histograms Bool
sat' TT _w _env = return True
sat' FF _w _env = return False
sat' (Prop p) w _env = return (p (w !! 0))
sat' (Neg p) w env = not <$> sat' p w env
sat' (X p) w env = sat' p (drop 1 w) env
sat' (U p q) w env = do
  xs <- filterM (\i -> sat' q (drop i w) env) [0 .. length w - 1]
  case listToMaybe xs of
    Nothing -> return False
    Just i -> and <$> sequence [sat' p (drop k w) env | k <- [0 .. i - 1]]
sat' (Always p) w env =
  and <$> sequence [sat' p (drop i w) env | i <- [0 .. length w - 1]]
sat' (Eventually p) w env =
  or <$> sequence [sat' p (drop i w) env | i <- [0 .. length w - 1]]
sat' (Or p q) w env = (||) <$> sat' p w env <*> sat' q w env
sat' (And p q) w env = do
  b <- sat' p w env
  if b then sat' q w env else return False
sat' (p :==> q) w env = do
  b <- sat' p w env
  if b then sat' q w env else return True
sat' (t1 :<= t2) _w env = return (eval t1 env <= eval t2 env)
sat' (t1 :== t2) _w env = return (eval t1 env == eval t2 env)
sat' (FreezeQuantifier x p) w env =
  sat' p w env {variables = Map.insert x (w !! 0) env.variables}
sat' (PercentileLessThan name _p t j) _w env = case eval t env of
  IntValue i -> do
    modify
      (Histograms . Map.adjust (measure (fromIntegral i)) name . histograms)
    return True -- NOTE: We defer the check until we got all statistics.
  _otherwise -> error "percentile less than of non-integer"

collect :: Form a -> [(Name, Percentile, Int)]
collect TT = []
collect FF = []
collect (Prop _p) = []
collect (Neg p) = collect p
collect (X p) = collect p
collect (U p q) = collect p ++ collect q
collect (Always p) = collect p
collect (Eventually p) = collect p
collect (Or p q) = collect p ++ collect q
collect (And p q) = collect p ++ collect q
collect (p :==> q) = collect p ++ collect q
collect (_ :<= _) = []
collect (_ :== _) = []
collect (FreezeQuantifier _x p) = collect p
collect (PercentileLessThan name ptile _t j) = [(name, ptile, j)]

sat'' :: Form Message -> [Message] -> Env Message -> IO Bool
sat'' formula w env =
  case runState (sat' formula w env) hists of
    (ok, hists') -> do
      mapM_ (uncurry prettyPrintHistogram) (Map.toList hists'.histograms)
      return (ok && meetsTargets targets hists')
  where
    targets = collect formula
    histogramNames = map (\(name, _, _) -> name) targets
    hists = emptyHistograms histogramNames

meetsTargets :: [(Name, Percentile, Int)] -> Histograms -> Bool
meetsTargets targets (Histograms hs) = go targets
  where
    go [] = True
    go ((name, p, target) : more) = case percentile p (hs Map.! name) of
      Nothing -> error ""
      Just i -> i <= fromIntegral target && go more

-- XXX: allow for 1% tolerance? due to histogram inaccuracy...

-- [](request -> <>(response /\ request.id == response.id /\ response.time - request.time <=p99 100ms)
example2 :: Form Message
example2 =
  Always $
    FreezeQuantifier "request" $
      Prop (\msg -> msg.kind == "request")
        :==> Eventually
          ( FreezeQuantifier "response" $
              Prop (\msg -> msg.kind == "response")
                `And` ( Var "response"
                          :. Id
                          :== Var "request"
                          :. Id
                      )
                `And` ( PercentileLessThan
                          "response time"
                          99.0
                          ( Var "response"
                              :. Time
                              :- Var "request"
                              :. Time
                          )
                          100
                      )
          )

-- NOTE: All request/response pairs need unique ids, otherwise the response
-- time gets measured multiple times (probably because of the `Always`?).
test2 :: IO Bool
test2 =
  let responseTime = 99
  in  and
        <$> sequence
          [ sat''
              example2
              ( [ Message {kind = "request", id = 0, time = 0}
                , Message {kind = "response", id = 0, time = 150}
                ]
                  ++ generateMessages 100 responseTime
              )
              emptyEnv
          , not
              <$> ( sat''
                      example2
                      ( [ Message {kind = "request", id = 0, time = 0}
                        , Message {kind = "response", id = 0, time = 150}
                        ]
                          ++ generateMessages 98 responseTime -- XXX: Why doesn't 99 messages work?
                      )
                      emptyEnv
                  )
          ]

generateMessages :: Int -> Int -> [Message]
generateMessages numberOfMessages duration = go [] 1 0
  where
    go acc n _time | n >= numberOfMessages + 1 = acc
    go acc n time
      | otherwise =
          go
            ( acc
                ++ [Message "request" n time, Message "response" n (time + duration)]
            )
            (n + 1)
            (time + duration)
