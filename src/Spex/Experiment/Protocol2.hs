module Spex.Experiment.Protocol2 (module Spex.Experiment.Protocol2) where

import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import Data.List (elemIndices, sort, union)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Debug.Trace

------------------------------------------------------------------------

type Proto state node msg = [Rule state node msg]

data Rule state node msg
  = Rule (state -> Bool) (state -> Session state node msg)

data Session state node msg
  = Send node msg (Session state node msg)
  | Recv node (msg -> Session state node msg)
  | Or (Session state node msg) (Session state node msg)
  | End state

data State = State
  { guesses :: Int
  , correct :: [Int]
  }
  deriving (Eq, Show)

initState :: State
initState = State 6 []

type Node = ()

data Msg = Guess Char | WrongGuess | LetterAppearsAt [Int] | Lost | Won
  deriving (Eq, Show)

------------------------------------------------------------------------

hangman :: String -> Proto State Node Msg
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

data MockError state node
  = NoTransitionInState state
  | SendingToWrongNode node node
  | ExpectingSendButGotRecv node
  | ExpectedRecvButGotSend node
  deriving (Eq, Show)

mock ::
  Proto state node msg
  -> state
  -> [msg]
  -> Either (MockError state node) [msg]
mock proto = go []
  where
    go replies state smsgs = case lookupSession state proto of
      [] -> Right replies
      [session] ->
        let (state', smsgs', rmsgs') = runSession session smsgs
        in  go (replies ++ rmsgs') state' smsgs'
      sessions -> undefined -- XXX: We need StdGen here to choose which rule to use.

lookupSession ::
  state -> Proto state node msg -> [Session state node msg]
lookupSession state = go []
  where
    go acc [] = reverse acc
    go acc (rule@(Rule pred k) : rules)
      | pred state = go (k state : acc) rules
      | otherwise = go acc rules

runSession ::
  Session state node msg -> [msg] -> (state, [msg], [msg])
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

------------------------------------------------------------------------

data TwoPCState = Init | Working | Aborted | Prepared | Committed
  deriving (Eq, Show)

data TwoPCNode = TM | RM1 | RM2 | Client
  deriving (Eq, Show)

data TwoPCMsg = Prepare | Write | Abort | Commit | Ack
  deriving (Eq, Show)

{-
-- Transaction manager
node TM where

init & Client?Write, RMs!prepare & working

working & RM_i?aborted, RMs!abort & aborted
working & RMs?prepared, RMs!commit & committed

-- Resource manager
node RM_{1,2} where

working & TM?prepare, TM!prepared & prepared
                    | TM!aborted & aborted

prepared & TM?commit & committed

prepared | aborted & TM?abort & aborted
 -}

twoPCProto ::
  TwoPCNode -> Proto TwoPCState TwoPCNode TwoPCMsg
twoPCProto Client =
  [ Rule (== Init) (\_s -> Send TM Write (End Working))
  , Rule (== Working) (\_s -> Recv TM (\Ack -> End Init))
  ]
twoPCProto TM =
  [ Rule
      (== Init)
      ( \_s ->
          Recv
            Client
            (\Write -> Send RM1 Prepare (Send RM2 Prepare (End Working)))
      )
  , Rule
      (== Working)
      (\_s -> Recv RM1 (\Abort -> Send RM2 Abort (End Aborted)))
  , -- XXX: symmetric...
    Rule
      (== Working)
      (\_s -> Recv RM2 (\Abort -> Send RM1 Abort (End Aborted)))
  , Rule
      (== Working)
      ( \_s ->
          Recv
            RM1
            ( \Prepare ->
                Recv
                  RM2
                  (\Prepare -> Send RM1 Commit (Send RM2 Commit (End Committed)))
            )
      )
  , -- XXX: Symmetric to the above
    Rule
      (== Working)
      ( \_s ->
          Recv
            RM2
            ( \Prepare ->
                Recv
                  RM1
                  (\Prepare -> Send RM1 Commit (Send RM2 Commit (End Committed)))
            )
      )
  , Rule
      (== Committed)
      ( \_s -> Recv RM1 (\Ack -> Recv RM2 (\Ack -> Send Client Ack (End Init)))
      )
  , -- XXX: symmetric
    Rule
      (== Committed)
      ( \_s -> Recv RM2 (\Ack -> Recv RM1 (\Ack -> Send Client Ack (End Init)))
      )
  ]
twoPCProto RM1 =
  [ Rule
      (== Working)
      ( \_s ->
          Recv
            TM
            ( \Prepare ->
                Send TM Prepare (End Prepared)
                  `Or` Send TM Abort (End Aborted)
            )
      )
  , Rule
      (== Prepared)
      (\_s -> Recv TM (\Commit -> Send TM Ack (End Working)))
  , Rule (== Prepared) (\_s -> Recv TM (\Abort -> End Aborted))
  , Rule (== Aborted) (\_s -> Recv TM (\Abort -> End Aborted))
  ]
twoPCProto RM2 =
  [ Rule
      (== Working)
      ( \_s ->
          Recv
            TM
            ( \Prepare ->
                Send TM Prepare (End Prepared)
                  `Or` Send TM Abort (End Aborted)
            )
      )
  , Rule
      (== Prepared)
      (\_s -> Recv TM (\Commit -> Send TM Ack (End Working)))
  , Rule (== Prepared) (\_s -> Recv TM (\Abort -> End Aborted))
  , Rule (== Aborted) (\_s -> Recv TM (\Abort -> End Aborted))
  ]

------------------------------------------------------------------------

lookupSessionTry ::
  state
  -> Maybe msg
  -> Proto state node msg
  -> IO [Session state node msg]
lookupSessionTry state mMsg = go []
  where
    go acc [] = return (reverse acc)
    go acc (rule@(Rule pred k) : rules)
      | pred state = do
          ok <- tryApply (k state) mMsg
          if ok
            then go (k state : acc) rules
            else go acc rules
      | otherwise = go acc rules

tryApply :: Session state node msg -> Maybe msg -> IO Bool
tryApply (Send _node _msg _session) (Just _msg') = return False
tryApply (Send _node _msg _session) Nothing = return True
tryApply (Recv _node k) (Just msg) =
  try (evaluate (k msg)) >>= \case
    Left (err :: PatternMatchFail) -> return False
    -- Left (err :: SomeException) -> return True
    Right _ -> return True
tryApply (Recv _node k) Nothing = return False
tryApply (Or _ _) (Just msg) = error "tryor"
tryApply (Or _ _) Nothing = error "tryor nothing"
tryApply (End _) _ = error "end"

mockSend ::
  (Eq node) =>
  Proto state node msg
  -> state
  -> node
  -> msg
  -> IO
      (Either (MockError state node) (Either (Session state node msg) state))
mockSend proto state node msg =
  lookupSessionTry state (Just msg) proto >>= \case
    [] -> return (Left (NoTransitionInState state))
    [session] -> return (sessionSend session node msg)
    (session : _sessions) -> return (sessionSend session node msg)

-- XXX: We need StdGen here to choose which rule to use.

sessionSend ::
  (Eq node) =>
  Session state node msg
  -> node
  -> msg
  -> Either (MockError state node) (Either (Session state node msg) state)
sessionSend (Recv node k) node' msg
  | node == node' = case k msg of
      End state' -> Right (Right state')
      _otherwise -> Right (Left (k msg))
  | otherwise = Left (SendingToWrongNode node node')
sessionSend _session node' msg = Left (ExpectedRecvButGotSend node')

mockRecv ::
  (Eq node) =>
  Proto state node msg
  -> state
  -> node
  -> IO
      ( Either
          (MockError state node)
          (Either (Session state node msg) state, msg)
      )
mockRecv proto state node =
  lookupSessionTry state Nothing proto >>= \case
    [] -> return (Left (NoTransitionInState state))
    [session] -> return (sessionRecv session node)
    sessions -> undefined -- XXX: We need StdGen here to choose which rule to use.

sessionRecv ::
  (Eq node) =>
  Session state node msg
  -> node
  -> Either
      (MockError state node)
      (Either (Session state node msg) state, msg)
sessionRecv (Send node msg session') node'
  | node == node' = case session' of
      End state' -> Right ((Right state'), msg)
      _otherwise -> Right ((Left session'), msg)
  | otherwise = Left (SendingToWrongNode node node')
sessionRecv (Or (Send node msg session) (Send node' msg' session')) node'' = error "or"
sessionRecv Recv {} node' = Left (ExpectingSendButGotRecv node')
sessionRecv _session node' = Left undefined

data Mock state node msg = Mock
  { sessionOrStateIORef :: IORef (Either (Session state node msg) state)
  , protocol :: Proto state node msg
  }

newMock ::
  Proto state node msg
  -> state
  -> IO (Mock state node msg)
newMock proto state =
  Mock
    <$> newIORef (Right state)
    <*> pure proto

-- send mock node msg = mock: node?msg
-- send tm Client Write = tm: Client?Write
-- ???
send ::
  (Eq node) =>
  Mock state node msg
  -> node
  -> msg
  -> IO (Either (MockError state node) ())
send mock node msg =
  readIORef mock.sessionOrStateIORef >>= \case
    Right state ->
      mockSend mock.protocol state node msg >>= \case
        Left mockError -> return (Left mockError)
        Right sessionOrState -> do
          writeIORef mock.sessionOrStateIORef sessionOrState
          return (Right ())
    Left session -> case sessionSend session node msg of
      Left mockError -> return (Left mockError)
      Right sessionOrState -> do
        writeIORef mock.sessionOrStateIORef sessionOrState
        return (Right ())

recv ::
  (Eq node) =>
  Mock state node msg
  -> node
  -> IO (Either (MockError state node) msg)
recv mock node =
  readIORef mock.sessionOrStateIORef >>= \case
    Right state ->
      mockRecv mock.protocol state node >>= \case
        Left mockError -> return (Left mockError)
        Right (sessionOrState, msg) -> do
          writeIORef mock.sessionOrStateIORef sessionOrState
          return (Right msg)
    Left session -> case sessionRecv session node of
      Left mockError -> return (Left mockError)
      Right (sessionOrState, msg) -> do
        writeIORef mock.sessionOrStateIORef sessionOrState
        return (Right msg)

unit_mockWin :: IO ()
unit_mockWin = do
  mock <- newMock (hangman "apa") initState
  send mock () (Guess 'a')
  Right (LetterAppearsAt [0, 2]) <- recv mock ()
  send mock () (Guess 'p')
  Right Won <- recv mock ()
  return ()

{-
-- Transaction manager
node TM where

init & Client?Write, RMs!prepare & working

working & RM_i?aborted, RMs!abort & aborted
working & RMs?prepared, RMs!commit & committed

-- Resource manager
node RM_{1,2} where

working & TM?prepare, TM!prepared & prepared
                    | TM!aborted & aborted

prepared & TM?commit & committed

prepared | aborted & TM?abort & aborted
 -}

-- glue :: Session state node msg -> Session state node msg ->

debugState :: Mock state node msg -> IO ()
debugState = undefined

data Channels node msg = Channels
  { incoming :: Map node (TQueue msg)
  , outgoing :: Map node (TQueue msg)
  }

recvCh :: (Ord node) => Channels node msg -> node -> IO msg
recvCh chs node = atomically (readTQueue (chs.incoming Map.! node))

sendCh :: (Ord node) => Channels node msg -> node -> msg -> IO ()
sendCh chs node msg = atomically (writeTQueue (chs.outgoing Map.! node) msg)

unit_2pc :: IO ()
unit_2pc = do
  client <- newMock (twoPCProto Client) Init
  tm <- newMock (twoPCProto TM) Init
  rm1 <- newMock (twoPCProto RM1) Working
  rm2 <- newMock (twoPCProto RM2) Working
  Right () <- send tm Client Write -- TM: Client?Write
  Right Prepare <- recv tm RM1 -- TM: RM1!Prepare
  Right Prepare <- recv tm RM2
  Right () <- send rm1 TM Prepare -- RM1: TM?Prepare
  Right () <- send rm2 TM Prepare
  Right () <- send tm RM1 Prepare -- TM: RM1?Prepare
  Right () <- send tm RM2 Prepare
  Right Commit <- recv tm RM1 -- TM: RM1!Commit
  Right Commit <- recv tm RM2
  reply <- send rm1 TM Commit -- RM1: TM?Commit
  print reply
  -- Right () <- send rm2 TM Commit

  -- Right () <- send rm1 TM Prepare -- TM: RM1!Prepare
  -- Right () <- send rm2 TM Prepare
  -- Right Prepare <- recv rm2 TM
  -- send rm1 TM Prepare
  -- send rm2 TM Prepare
  -- send tm RM1 Commit
  -- send tm RM2 Commit
  -- Right Commit <- recv rm1 TM
  -- Right Commit <- recv rm2 TM
  -- send rm1 TM Ack
  -- send rm2 TM Ack
  -- Right Ack <- recv tm RM1
  -- Right Ack <- recv tm RM2
  -- send tm Client Ack
  -- Right Ack <- recv tm Client
  return ()

------------------------------------------------------------------------

data SyncApi req resp = SyncApi [Op req resp]

data Op req resp = Op Method Path req resp

data Method = Post
type Path = String

data Deployment node = Deployment
  { topology :: Map node [node]
  , clientFacing :: [node]
  , clientApi :: SyncApi TwoPCMsg TwoPCMsg
  }

deploy :: Deployment node -> node -> IO ()
deploy = undefined
