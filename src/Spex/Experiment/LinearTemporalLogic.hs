module Spex.Experiment.LinearTemporalLogic (module Spex.Experiment.LinearTemporalLogic) where

-- Bug Catching: Automated Program Verification:
--   https://www.cs.cmu.edu/~15414/
--   https://www.cs.cmu.edu/~15414/s23/lectures/24-ltl.pdf
--   NuSMV: https://www.cs.cmu.edu/~15414/lectures/21-ltl.pdf
--   The algorithm used in the 2018 lectures:
--     https://link.springer.com/content/pdf/10.1007/978-0-387-34892-6_1.pdf
-- https://en.wikipedia.org/wiki/Property_Specification_Language

import Data.Maybe

------------------------------------------------------------------------

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

sat :: (Eq a) => Form a -> [a] -> Bool
sat TT _w = True
sat FF _w = False
sat (Prop p) w = p (w !! 0)
sat (Neg p) w = not (sat p w)
sat (X p) w = sat p (drop 1 w)
sat (U p q) w =
  case listToMaybe [i | i <- [0 .. length w - 1], sat q (drop i w)] of
    Nothing -> False
    Just i -> and [sat p (drop k w) | k <- [0 .. i - 1]]
sat (Always p) w = and [sat p (drop i w) | i <- [0 .. length w - 1]]
sat (Eventually p) w = or [sat p (drop i w) | i <- [0 .. length w - 1]]
sat (Or p q) w = sat p w || sat q w
sat (And p q) w = sat p w && sat q w

eventually :: Form a -> Form a
eventually p = Eventually p -- TT `U` p

always :: Form a -> Form a
always p = Always p -- Neg (eventually (Neg p))

release :: Form a -> Form a -> Form a
release p q = Neg ((Neg p) `U` (Neg q))

(==>) :: Form a -> Form a -> Form a
p ==> q = Neg p `Or` q

------------------------------------------------------------------------

-- Some examples are taken from:
-- https://www.cl.cam.ac.uk/archive/mjcg/TempLogic/Lectures/L4.Jan27.pdf

t0 = sat (always (Prop (== 1))) [1, 1, 1]

-- “Eventually the state becomes permanently Done“
-- F(G Done)
t1 = sat (eventually (always (Prop (== 1)))) [0, 0, 0, 1, 1, 1]

-- XXX:
-- “DeviceEnabled holds infinitely often along every path”
-- G(F DeviceEnabled)
t2 = sat (always (eventually (Prop (== 1)))) undefined

-- “Every Req is followed by an Ack”
-- G(Req ⇒ F Ack)
-- Number of Req and Ack may differ - no counting
t3 =
  sat
    ( always
        ( (Prop (== "Req"))
            ==> eventually (Prop (== "Ack"))
        )
    )
    [ "a"
    , "Req"
    , "b"
    , "c"
    , "Ack" -- NOTE: changing this doesn't break the property...
    , "d"
    , "Req"
    , "e"
    , "Ack"
    ]

-- XXX: How is this different from Req-Ack?
-- “If Enabled infinitely often then Running infinitely often”
-- G(F Enabled) ⇒ G(F Running)
t4 =
  sat
    ( always (eventually (Prop (== "Enabled")))
        ==> always (eventually (Prop (== "Running")))
    )
    ["Enabled", ".", "Running", ".", "Enabled", "Running"]

-- “An upward going lift at the second floor keeps going up if a passenger
-- requests the fifth floor” G(AtFloor2 ∧ DirectionUp ∧ RequestFloor5 ⇒
-- [DirectionUp U AtFloor5])
data Dir = Up | Down
  deriving (Eq, Show)

data Elevator = Elevator
  { currentFloor :: Int
  , direction :: Dir
  , requestedFloor :: Int
  }
  deriving (Eq)

unit_elevator =
  sat
    ( always $
        (atFloor2 `And` directionUp `And` requestFloor5)
          ==> (directionUp `U` atFloor5)
    )
    [ Elevator 2 Up 5
    , Elevator 3 Up 5
    , Elevator 4 Up 5
    , Elevator 5 Up 5
    , Elevator 5 Down 4
    , Elevator 4 Down 4
    ]
  where
    atFloor2 :: Form Elevator
    atFloor2 = Prop (\e -> e.currentFloor == 2)

    directionUp :: Form Elevator
    directionUp = Prop (\e -> e.direction == Up)

    requestFloor5 :: Form Elevator
    requestFloor5 = Prop (\e -> e.requestedFloor == 5)

    atFloor5 :: Form Elevator
    atFloor5 = Prop (\e -> e.currentFloor == 5)
