module Spex.Experiment.Protocol3 (module Spex.Experiment.Protocol3) where

import Spex.Experiment.LinearTemporalLogic hiding (Or)

import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import Data.List ((\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Random

------------------------------------------------------------------------

type Topology node = Map node [node]

data Node = TM | RM1 | RM2 | Client
  deriving (Eq, Ord, Show)

twoPCTopology :: Topology Node
twoPCTopology =
  Map.fromList
    [ (TM, [RM1, RM2])
    , (RM1, [TM])
    , (RM2, [TM])
    ]

data Protocol state node msg = Protocol [Transition state node msg]

data Transition state node msg
  = Transition (state -> Bool) (Session state node msg)

data Session state node msg
  = Send [node] msg (Session state node msg)
  | Recv [node] (msg -> Maybe (Session state node msg))
  | Or (Session state node msg) (Session state node msg)
  | End state

only :: (Eq msg) => msg -> (msg -> a) -> (msg -> Maybe a)
only msg k = \msg' -> if msg == msg' then Just (k msg) else Nothing

recv ::
  (Eq msg) =>
  node
  -> msg
  -> (msg -> Session state node msg)
  -> Session state node msg
recv node msg k = recvMany [node] msg k

recvMany ::
  (Eq msg) =>
  [node]
  -> msg
  -> (msg -> Session state node msg)
  -> Session state node msg
recvMany nodes msg k = Recv nodes (only msg k)

recvAny ::
  (Eq msg) =>
  [node]
  -> msg
  -> (msg -> Session state node msg)
  -> Session state node msg
recvAny [] _msg _k = error "recvAny: empty list"
recvAny (node : nodes) msg k = go (Recv [node] (only msg k)) nodes
  where
    go session [] = session
    go session (node : nodes) = go (session `Or` Recv [node] (only msg k)) nodes

data SomeProtocol node msg
  = forall state.
    (Show state) =>
    SomeProtocol (Protocol state node msg) state

data TMState = TMInit | TMWorking | TMCommitted | TMAborted
  deriving (Eq, Show)

data RMState = RMInit | RMPrepared | RMAborted
  deriving (Eq, Show)

data ClientState = ClientInit
  deriving (Eq, Show)

data Msg
  = Write
  | Prepare
  | Prepared
  | Commit
  | Comitted
  | Abort
  | Aborted
  | Ack
  deriving (Eq, Show)

tmProtocol :: Protocol TMState Node Msg
tmProtocol =
  Protocol
    [ Transition
        (== TMInit)
        ( recv
            Client
            Write
            (\Write -> Send [RM1, RM2] Prepare (End TMWorking))
        )
    , Transition
        (== TMWorking)
        ( recvMany
            [RM1, RM2]
            Prepared
            ( \Prepared ->
                Send [RM1, RM2] Commit (End TMCommitted)
            )
        )
    , Transition
        (== TMCommitted)
        ( recvMany
            [RM1, RM2]
            Ack
            ( \Ack -> recvMany [RM1, RM2] Ack (\Ack -> Send [Client] Ack (End TMInit))
            )
        )
    , Transition
        (== TMWorking)
        ( recvAny
            [RM1, RM2]
            Aborted
            (\Aborted -> Send [RM1, RM2, Client] Abort (End TMAborted))
        )
    ]

rmProtocol :: Protocol RMState Node Msg
rmProtocol =
  Protocol
    [ Transition
        (== RMInit)
        ( recv
            TM
            Prepare
            ( \Prepare ->
                Send [TM] Prepared (End RMPrepared)
                  `Or` Send [TM] Aborted (End RMAborted)
            )
        )
    , Transition
        (== RMPrepared)
        (recv TM Commit (\Commit -> Send [TM] Ack (End RMInit)))
    , Transition (== RMPrepared) (recv TM Abort (\Abort -> End RMAborted))
    , Transition (== RMAborted) (recv TM Abort (\Abort -> End RMAborted))
    ]

clientProtocol :: Protocol ClientState Node Msg
clientProtocol =
  Protocol
    [ Transition
        (== ClientInit)
        ( recv TM Ack (\Ack -> End ClientInit)
            `Or` recv TM Abort (\Abort -> End ClientInit)
        )
    ]

data Deployment node msg = Deployment
  { topology :: Topology node -- NOTE: not used yet...
  , protocols :: Map node (SomeProtocol node msg)
  }

twoPCDeployment :: Deployment Node Msg
twoPCDeployment =
  Deployment
    { topology = twoPCTopology
    , protocols =
        Map.fromList
          [ (TM, SomeProtocol tmProtocol TMInit)
          , (RM1, SomeProtocol rmProtocol RMInit)
          , (RM2, SomeProtocol rmProtocol RMInit)
          , (Client, SomeProtocol clientProtocol ClientInit)
          ]
    }

data Mock state node msg = Mock
  { protocol :: Protocol state node msg
  , state :: Either (Session state node msg) state
  , alternatives :: [Session state node msg]
  }

newMock ::
  Protocol state node msg -> state -> Mock state node msg
newMock protocol state = Mock protocol (Right state) []

runMock ::
  (Eq node, Show state, Show node, Show msg) =>
  Mock state node msg
  -> Maybe (Envelope node msg)
  -> StdGen
  -> (Mock state node msg, [(node, msg)])
runMock mock mEnvelope stdGen = case mock.state of
  Left session -> runSession mock mEnvelope session stdGen [] mock.alternatives
  Right state -> case lookupState state mock.protocol of
    [] -> (mock, [])
    (session : sessions) -> runSession mock mEnvelope session stdGen [] sessions

runSession ::
  (Eq node, Show state, Show node, Show msg) =>
  Mock state node msg
  -> Maybe (Envelope node msg)
  -> Session state node msg
  -> StdGen
  -> [(node, msg)]
  -> [Session state node msg]
  -> (Mock state node msg, [(node, msg)])
runSession mock Nothing session@(Recv nodes k) stdGen sentMsgs alternatives =
  ( mock
      { state = Left session
      , alternatives = alternatives ++ mock.alternatives
      }
  , sentMsgs
  )
runSession mock (Just envelope) (Recv nodes k) stdGen sentMsgs alternatives
  | envelope.from `notElem` nodes = case alternatives of
      [] -> error $ "runSession: deadlock, envelope = " <> show envelope
      (session : sessions) -> runSession mock (Just envelope) session stdGen sentMsgs sessions
  | otherwise = case k envelope.content of
      Nothing -> case alternatives of
        [] ->
          error $
            "runSession: deadlock, envelope = "
              <> show envelope
              <> "\nsentMsgs = "
              <> show sentMsgs
              <> "\nsession = "
              <> "Recv "
              <> show nodes
              <> "\nstate = "
              <> case mock.state of
                Right state -> show state
                Left session -> "Left session"
        (session : sessions)
          | null (nodes \\ [envelope.from]) ->
              runSession mock Nothing session stdGen sentMsgs sessions
          | otherwise ->
              runSession
                mock
                Nothing
                (Recv (nodes \\ [envelope.from]) k)
                stdGen
                sentMsgs
                sessions
      Just session -> runSession mock Nothing session stdGen sentMsgs alternatives
runSession mock mEnvelope (Send nodes msg session') stdGen sentMsgs alternatives =
  runSession
    mock
    mEnvelope
    session'
    stdGen
    (sentMsgs ++ zip nodes (repeat msg))
    alternatives
runSession mock mEnvelope (Or session session') stdGen sentMsgs alternatives =
  let (bool, stdGen') = random stdGen
  in  if bool
        then
          runSession
            mock
            mEnvelope
            session
            stdGen'
            sentMsgs
            (session' : alternatives)
        else
          runSession
            mock
            mEnvelope
            session'
            stdGen'
            sentMsgs
            (session : alternatives)
runSession mock mEnvelope (End state') _stdGen sentMsgs _altenatives =
  (mock {state = Right state'}, sentMsgs)

lookupState ::
  state
  -> Protocol state node msg
  -> [Session state node msg]
lookupState state (Protocol transitions) = go transitions
  where
    go [] = []
    go (Transition precondition session : transitions')
      | precondition state = session : go transitions'
      | otherwise = go transitions'

data SomeMock node msg
  = forall state. (Show state) => SomeMock (Mock state node msg)

data Sim node msg = Sim
  { nodes :: Map node (SomeMock node msg)
  , messages :: [Envelope node msg]
  , prng :: StdGen
  , trace :: [Envelope node msg]
  }

data Envelope node msg = Envelope
  { from :: node
  , to :: node
  , content :: msg
  -- , arrivalTime :: UTCTime
  }
  deriving (Eq, Show)

prettyEnvelope ::
  (Show node, Show msg) => Envelope node msg -> String
prettyEnvelope envelope =
  show envelope.from
    <> " -- "
    <> show envelope.content
    <> " --> "
    <> show envelope.to

newSim ::
  forall node msg.
  (Ord node) =>
  Deployment node msg
  -> [Envelope node msg]
  -> Int
  -> IO (Sim node msg)
newSim deployment initialMessages seed = do
  return
    Sim
      { nodes = Map.fromList (makeMocks (Map.toList deployment.protocols))
      , messages = initialMessages
      , prng = mkStdGen seed
      , trace = []
      }
  where
    makeMocks ::
      [(node, SomeProtocol node msg)] -> [(node, SomeMock node msg)]
    makeMocks [] = []
    makeMocks ((node, SomeProtocol protocol initialState) : rest) =
      (node, SomeMock (newMock protocol initialState)) : makeMocks rest

stepSim ::
  (Ord node, Show node, Show msg) =>
  Sim node msg
  -> Either (Sim node msg) (Sim node msg)
stepSim sim = case sim.messages of
  [] -> Right sim
  envelope : envelopes -> case sim.nodes Map.! envelope.to of
    SomeMock mock ->
      let (stdGen', stdGen'') = split sim.prng
          (mock', sentMsgs) = runMock mock (Just envelope) stdGen'
      in  Left
            Sim
              { nodes = Map.insert envelope.to (SomeMock mock') sim.nodes
              , messages = envelopes ++ map (uncurry (Envelope envelope.to)) sentMsgs
              , prng = stdGen''
              , trace = envelope : sim.trace
              }

runSim ::
  (Ord node, Show node, Show msg) => Sim node msg -> [Envelope node msg]
runSim sim = case stepSim sim of
  Right sim' -> reverse sim'.trace
  Left sim' -> runSim sim'

test :: Int -> IO Bool
test seed = do
  sim <-
    newSim
      twoPCDeployment
      [Envelope {from = Client, to = TM, content = Write}]
      seed
  let trace = runSim sim
  if sat
    ( always
        ( Prop (\e -> e.from == Client && e.content == Write)
            ==> eventually
              ( Prop (\e -> e.to == Client && (e.content == Ack) || e.content == Abort)
              )
        )
    )
    trace
    then return True
    else do
      putStrLn $ "False, seed: " ++ show seed
      mapM_ (putStrLn . prettyEnvelope) (runSim sim)
      return False

t :: Int -> IO ()
t numberOfTests = do
  seed <- randomIO
  passed <- test seed
  if passed
    then t (numberOfTests - 1)
    else return ()
