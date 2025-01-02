module Spex.Experiment.TimedPropositionalTemporalLogic (module Spex.Experiment.TimedPropositionalTemporalLogic) where

-- TPLT from "A Really Temporal Logic" (1994):
-- https://www.seas.upenn.edu/~alur/JACM94.pdf

import Data.Fixed
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Numeric.Natural

------------------------------------------------------------------------

infixl 6 :+
infix 4 :<=

type Var = String

-- TODO: Currently the natural stands for seconds, we probably want something
-- smaller to be able to be more precise.
data Constraint
  = Var :+ Natural
  | RelativeTime Natural

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
  | Constraint :<= Constraint
  | Congurent Constraint Constraint Natural -- XXX: I'm not sure how to use this...
  | FreezeQuantifier Var (Form a)

-- <>x.(p /\ x <= 10)
example :: Form Bool
example =
  Eventually $
    FreezeQuantifier "x" $
      Prop (== True) `And` ("x" :+ 0 :<= RelativeTime 10)

type Env time = Map Var time

class Time t where
  (.==) :: t -> t -> Bool
  (.<=) :: t -> t -> Bool
  (.+) :: t -> Natural -> t
  (.-) :: t -> t -> t
  (.%) :: t -> Natural -> t
  fromNatural :: Natural -> t

instance Time Natural where
  t0 .== t1 = t0 == t1
  t0 .<= t1 = t0 <= t1
  t0 .+ t1 = t0 + t1
  t0 .- t1 = t0 - t1
  t0 .% t1 = t0 `mod` t1
  fromNatural = id

instance Time UTCTime where
  t0 .== t1 = t0 == t1
  t0 .<= t1 = t0 <= t1
  t0 .+ n = fromIntegral n `addUTCTime` t0
  t0 .- t1 = posixSecondsToUTCTime (t0 `diffUTCTime` t1)
  t0 .% n =
    posixSecondsToUTCTime $
      secondsToNominalDiffTime $
        nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t0)
          `mod'` fromIntegral n
  fromNatural = posixSecondsToUTCTime . fromIntegral

-- NOTE: We need t0 here in order to compute the relative time to when the
-- trace started. For time := Natural isn't not needed as t0 = 0, but for
-- timestamps we need the supply the start time.
lookupEnv :: (Time time) => Constraint -> Env time -> time
lookupEnv (x :+ c) env = env Map.! x .+ c
lookupEnv (RelativeTime c) env = env Map.! "t0" .+ fromNatural c

-- TODO: Would be interesting to generalise this from a single trace to lists
-- of traces, and make assertions about percentiles, e.g. a request is always
-- eventually responded to within 200ms in p99, i.e. 1% of requests may be
-- slower.

sat ::
  (Time time) =>
  Form a
  -> [(time, a)]
  -> Env time
  -> Bool
sat TT _w _env = True
sat FF _w _env = False
sat (Prop p) w _env = p (snd (w !! 0))
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
sat (c1 :<= c2) _w env = lookupEnv c1 env .<= lookupEnv c2 env
-- XXX: Not sure if Congurent is right, or how to use it, or if it works with
-- timestamps as opposed to naturals as time...
sat (Congurent c1 c2 d) _w env = (lookupEnv c1 env .- lookupEnv c2 env) .% d .== fromNatural 0
sat (FreezeQuantifier x p) w env = sat p w (Map.insert x t0 env)
  where
    t0 = fst (w !! 0)

------------------------------------------------------------------------

test0 :: Bool
test0 =
  sat
    example
    ( [(0, False), (1, False), (2, False), (9, False), (10, True)] ::
        [(Natural, Bool)]
    )
    (Map.singleton "t0" 0)
    == True

test1 :: Bool
test1 =
  sat
    example
    ( [(0, False), (1, False), (2, False), (9, False), (11, True)] ::
        [(Natural, Bool)]
    )
    (Map.singleton "t0" 0)
    == False

test2 :: IO Bool
test2 = do
  now <- getCurrentTime
  let t1 = now .+ 1
      t2 = now .+ 2
      t9 = now .+ 9
      t10 = now .+ 10
  print now
  print t1
  print t10
  return $
    sat
      example
      [(now, False), (t1, False), (t2, False), (t9, False), (t10, True)]
      (Map.singleton "t0" now)
      == True

test3 :: IO Bool
test3 = do
  now <- getCurrentTime
  let t1 = now .+ 1
      t2 = now .+ 2
      t9 = now .+ 9
      t11 = now .+ 11
  print now
  print t11
  return $
    sat
      example
      [(now, False), (t1, False), (t2, False), (t9, False), (t11, True)]
      (Map.singleton "t0" now)
      == False

test :: IO ()
test = do
  b2 <- test2
  b3 <- test3
  let bools = [test0, test1, b2, b3]
  if and bools then putStrLn "Passed" else print bools
