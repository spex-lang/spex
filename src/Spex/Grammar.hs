module Spex.Grammar (module Spex.Grammar) where

-- https://www.cse.chalmers.se/~coquand/AUTOMATA/w6.html
-- https://en.wikipedia.org/wiki/Augmented_Backus%E2%80%93Naur_form
-- https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form

------------------------------------------------------------------------

-- * Regex

--
-- https://www.cse.chalmers.se/~coquand/AUTOMATA/w3.html

data Reg a
  = Empty
  | Epsilon
  | Atom a
  | Plus (Reg a) (Reg a)
  | Concat (Reg a) (Reg a)
  | Star (Reg a)
  | Inter (Reg a) (Reg a)
  | Compl (Reg a)

isEmpty :: Reg a -> Bool
isEmpty Empty = True
isEmpty (Plus e1 e2) = isEmpty e1 && isEmpty e2
isEmpty (Concat e1 e2) = isEmpty e1 || isEmpty e2
isEmpty _ = False

hasEpsilon :: Reg a -> Bool
hasEpsilon Epsilon = True
hasEpsilon (Star _) = True
hasEpsilon (Plus e1 e2) = hasEpsilon e1 || hasEpsilon e2
hasEpsilon (Concat e1 e2) = hasEpsilon e1 && hasEpsilon e2
hasEpsilon _ = False

atMostEps :: Reg a -> Bool
atMostEps Empty = True
atMostEps Epsilon = True
atMostEps (Star e) = atMostEps e
atMostEps (Plus e1 e2) = atMostEps e1 && atMostEps e2
atMostEps (Concat e1 e2) = isEmpty e1 || isEmpty e2 || (atMostEps e1 && atMostEps e2)
atMostEps _ = False

infinite :: Reg a -> Bool
infinite (Star e) = not (atMostEps e)
infinite (Plus e1 e2) = infinite e1 || infinite e2
infinite (Concat e1 e2) = (infinite e1 && notEmpty e2) || (notEmpty e1 && infinite e2)
infinite _ = False

notEmpty :: Reg a -> Bool
notEmpty e = not (isEmpty e)

der :: (Eq a) => a -> Reg a -> Reg a
der b (Atom b1) = if b == b1 then Epsilon else Empty
der b (Plus e1 e2) = Plus (der b e1) (der b e2)
der b (Concat e1 e2) | hasEpsilon e1 = Plus (Concat (der b e1) e2) (der b e2)
der b (Concat e1 e2) = Concat (der b e1) e2
der b (Star e) = Concat (der b e) (Star e)
der b (Inter e1 e2) = Inter (der b e1) (der b e2)
der b (Compl e) = Compl (der b e)
der b _ = Empty

isIn :: (Eq a) => [a] -> Reg a -> Bool
isIn [] e = hasEpsilon e
isIn (a : as) e = isIn as (der a e)

gen :: Reg a -> [a]
gen Empty = []
gen Epsilon = undefined
gen (Atom x) = [x]
gen (Plus e1 e2) = undefined
gen (Concat e1 e2) = gen e1 ++ gen e2
