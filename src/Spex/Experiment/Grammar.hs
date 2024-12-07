{-# LANGUAGE OverloadedStrings #-}

module Spex.Experiment.Grammar (module Spex.Experiment.Grammar) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.List (intersect)
import Data.String

import Spex.Verifier.Generator
import Spex.Generator.Prng
import Spex.Generator.Combinator

-- https://www.cse.chalmers.se/~coquand/AUTOMATA/w6.html
-- https://en.wikipedia.org/wiki/Augmented_Backus%E2%80%93Naur_form
-- https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form

------------------------------------------------------------------------

-- * Regex

--
-- https://www.cse.chalmers.se/~coquand/AUTOMATA/w3.html
-- https://hackage.haskell.org/package/regex-applicative-0.3.4/docs/src/Text.Regex.Applicative.Types.html#RE

data Reg a
  = Empty
  | Epsilon
  | Atom a
  | Plus (Reg a) (Reg a)
  | Concat (Reg a) (Reg a)
  | Star (Reg a)
  | Inter (Reg a) (Reg a)
  | Compl (Reg a)

instance Semigroup (Reg a) where
  (<>) = Concat

instance Monoid (Reg a) where
  mempty = Epsilon
  mappend = (<>)

instance (char ~ Char) => IsString (Reg char) where
  fromString = string

string :: [a] -> Reg a
string = mconcat . map Atom

instance Functor Reg where
  fmap f re = case re of
    Empty -> Empty
    Epsilon -> Epsilon
    Atom x -> Atom (f x)
    Plus e1 e2 -> Plus (fmap f e1) (fmap f e2)
    Concat e1 e2 -> Concat (fmap f e1) (fmap f e2)
    Star e -> Star (fmap f e)
    Inter e1 e2 -> Inter (fmap f e1) (fmap f e2)
    Compl e -> Compl (fmap f e)

instance Applicative Reg where
  pure = Atom
  f <*> x = undefined

instance Alternative Reg where
  empty = Empty
  (<|>) = Plus

-- some re = undefined -- re <> many re

some :: Reg a -> Reg a
some re = re <> many re

many :: Reg a -> Reg a
many = Star

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

isSubSetOf :: Reg a -> Reg a -> Bool
isSubSetOf e1 e2 = isEmpty (e1 `Inter` (Compl e2))

gen :: (Eq a) => Reg a -> Gen [a]
gen re | notEmpty re = case re of
  Empty -> error "empty"
  Epsilon -> return []
  Atom x -> return [x]
  Plus e1 e2 -> do
    b <- chooseBounded
    if b then gen e1 else gen e2
  Concat e1 e2 ->
    (++) <$> gen e1 <*> gen e2
  Star e -> do
    n <- choose (0, 10)
    fmap concat (replicateM n (gen e))
  Compl e -> undefined
  Inter e1 e2 -> intersect <$> gen e1 <*> gen e2

prop_gen :: (Monoid a, Eq a) => Int -> Size -> Reg a -> Bool
prop_gen seed size re = 
  snd (runGen (gen re) (mkPrng seed) size) `isIn` re

testRe :: Reg Char
testRe = "ab" <|> "c" <|> many "*" <|> some "+"

-- Aka ?
possibly :: Reg a -> Reg a
possibly re = re <|> mempty

exactly :: Reg a -> Int -> Reg a
exactly re n = mconcat (replicate n re)

atleast :: Reg a -> Int -> Reg a
atleast re 0 = many re
atleast re n = re <> atleast re (n - 1)

atmost :: Reg a -> Int -> Reg a
atmost _re 0 = mempty
atmost re n = (re <> atmost re (n - 1)) <|> mempty

between :: Reg a -> Int -> Int -> Reg a
between re lo0 hi0 = go lo0 hi0
  where
    go 0 hi = atmost re (hi - lo0)
    go lo hi = re <> go (lo - 1) hi

-- range 'a' 'z' = [a-z]
range :: (Enum a) => a -> a -> Reg a
range from to = asum (map Atom [from .. to])

testReColour :: Reg Char
testReColour = between ("colo" <> possibly "u" <> "r") 1 3

-- XXX: this is biased, because of the nested Plus... Introduce separate Asum
-- constructor?
wildcard :: Reg Char
wildcard = range 'a' 'z' -- minBound .. maxBound?

testRe2 :: Reg Char
testRe2 = Atom 'a' <> wildcard <> Atom 'c'

test :: Prng -> Size -> String
test prng size = snd (runGen (gen testReColour) prng size)

t :: IO ()
t = do
  replicateM_ 10 $ do
    (prng, _seed) <- newPrng Nothing
    let size = 10
    putStrLn (test prng size)

------------------------------------------------------------------------

-- * BNF

newtype Syntax = Syntax [Rule]

-- <name> ::= e_1 | ... | e_n
data Rule = Or RuleName [Expr]

type Text = String

newtype RuleName = RuleName Text

-- <name>
-- "lit"
-- <name_1> "lit_1" <name_2>
-- "lit_2"*
-- ("lit_3" <name_1>)*
data Expr = Term Term | And [Expr] | StarB Expr

data Term = Literal Text | Rule RuleName

plusB :: Expr -> Expr
plusB e = And [e, StarB e]
