module Spex.Experiment.Protocol2 (module Spex.Experiment.Protocol2) where

import Data.List (elemIndices, sort, union)

------------------------------------------------------------------------

type Proto state node smsg rmsg = [Rule state node smsg rmsg]

data Rule state node smsg rmsg
  = Rule (state -> Bool) (state -> Session state node smsg rmsg)

data Session state node smsg rmsg
  = Send node smsg (Session state node smsg rmsg)
  | Recv node (rmsg -> Session state node smsg rmsg)
  | End state

data State = State
  { guesses :: Int
  , correct :: [Int]
  }
  deriving (Eq, Show)

initState :: State
initState = State 6 []

type Node = ()

data RMsg = Guess Char
  deriving (Eq, Show)

data SMsg = WrongGuess | LetterAppearsAt [Int] | Lost | Won
  deriving (Eq, Show)

------------------------------------------------------------------------

hangman :: String -> Proto State Node SMsg RMsg
hangman word =
  [ Rule
      (\s -> s.guesses > 0)
      ( \s ->
          Recv
            ()
            ( \(Guess letter) ->
                case elemIndices letter word of
                  []
                    | s.guesses == 1 -> Send () Lost (End s {guesses = 0})
                    | otherwise -> Send () WrongGuess (End s {guesses = s.guesses - 1})
                  is
                    | sort (s.correct `union` is) == [0 .. length word - 1] ->
                        Send () Won (End s {guesses = 0})
                    | otherwise ->
                        Send () (LetterAppearsAt is) (End s {correct = s.correct `union` is})
            )
      )
  ]

data MockError state = NoRulesForState state
  deriving (Eq, Show)

mock ::
  Proto state node rmsg smsg
  -> state
  -> [smsg]
  -> Either (MockError state) [rmsg]
mock proto = go []
  where
    go replies state smsgs = case lookupSession state proto of
      [] -> Right replies
      [session] ->
        let (state', smsgs', rmsgs) = runSession session smsgs
        in  go (replies ++ rmsgs) state' smsgs'
      sessions -> undefined -- XXX: We need StdGen here to choose which rule to use.

lookupSession ::
  state -> Proto state node smsg rmsg -> [Session state node smsg rmsg]
lookupSession state = go []
  where
    go acc [] = reverse acc
    go acc (rule@(Rule pred k) : rules)
      | pred state = go (k state : acc) rules
      | otherwise = go acc rules

runSession ::
  Session state node rmsg smsg -> [smsg] -> (state, [smsg], [rmsg])
runSession = go []
  where
    go replies (Recv _node k) [] = error "Need more input"
    go replies (Recv _node k) (smsg : smsgs) = go replies (k smsg) smsgs
    go replies (Send _node rmsg session') smsgs = go (rmsg : replies) session' smsgs
    go replies (End s) smsgs = (s, smsgs, reverse replies)

------------------------------------------------------------------------

unit_won :: Bool
unit_won =
  mock (hangman "apa") initState [Guess 'a', Guess 'b', Guess 'p']
    == Right [LetterAppearsAt [0, 2], WrongGuess, Won]

unit_lost :: Bool
unit_lost =
  mock (hangman "apa") initState (map Guess "bcdefg")
    == Right (replicate 5 WrongGuess ++ [Lost])
