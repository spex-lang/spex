module Spex.Experiment.Protocol3 (module Spex.Experiment.Protocol3) where

import Control.Concurrent.STM
import Control.Exception
import Data.IORef
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
  | Recv [node] (msg -> Session state node msg)
  | Or (Session state node msg) (Session state node msg)
  | End state

data SomeProtocol node msg
  = forall state.
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
        ( Recv
            [Client]
            (\Write -> Send [RM1, RM2] Prepare (End TMWorking))
        )
    , Transition
        (== TMWorking)
        ( Recv
            [RM1, RM2]
            ( \Prepared ->
                Recv
                  [RM1, RM2]
                  (\Prepared -> Send [RM1, RM2] Commit (End TMCommitted))
            )
        )
    , Transition
        (== TMCommitted)
        ( Recv
            [RM1, RM2]
            (\Ack -> Recv [RM1, RM2] (\Ack -> Send [Client] Ack (End TMInit)))
        )
    , Transition
        (== TMWorking)
        ( Recv
            [RM1, RM2]
            (\Aborted -> Send [RM1, RM2, Client] Abort (End TMAborted))
        )
    , Transition
        (== TMCommitted)
        ( Recv
            [RM1, RM2]
            (\Ack -> Recv [RM1, RM2] (\Ack -> Send [Client] Ack (End TMInit)))
        )
    ]

rmProtocol :: Protocol RMState Node Msg
rmProtocol =
  Protocol
    [ Transition
        (== RMInit)
        ( Recv
            [TM]
            ( \Prepare ->
                Send [TM] Prepared (End RMPrepared)
                  `Or` Send [TM] Aborted (End RMAborted)
            )
        )
    , Transition
        (== RMPrepared)
        (Recv [TM] (\Commit -> Send [TM] Ack (End RMInit)))
    , Transition (== RMPrepared) (Recv [TM] (\Abort -> End RMAborted))
    , Transition (== RMAborted) (Recv [TM] (\Abort -> End RMAborted))
    ]

clientProtocol :: Protocol ClientState Node Msg
clientProtocol =
  Protocol
    [ Transition
        (== ClientInit)
        ( Recv [TM] (\Ack -> End ClientInit)
            `Or` Recv [TM] (\Abort -> End ClientInit)
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
  }

newMock ::
  Protocol state node msg -> state -> Mock state node msg
newMock protocol state = Mock protocol (Right state)

runMock ::
  forall state node msg.
  (Eq node, Show node) =>
  Mock state node msg
  -> Envelope node msg
  -> StdGen
  -> (Mock state node msg, [(node, msg)])
runMock mock envelope stdGen = case mock.state of
  Left session -> runSession session stdGen []
  Right state -> case lookupState state mock.protocol of
    [] -> (mock, [])
    [session] -> runSession session stdGen []
    sessions ->
      let (index, stdGen') = randomR (0, length sessions - 1) stdGen
      in  runSession (sessions !! index) stdGen' []
  where
    runSession ::
      Session state node msg
      -> StdGen
      -> [(node, msg)]
      -> (Mock state node msg, [(node, msg)])
    runSession (Recv nodes k) stdGen' sentMsgs
      | envelope.from `notElem` nodes =
          error $
            "runMock: node mismatch, "
              ++ show envelope.from
              ++ " `notElem` "
              ++ show nodes
      | otherwise = runSession (k envelope.content) stdGen' sentMsgs
    runSession (Send nodes msg session') stdGen' sentMsgs = runSession session' stdGen' (sentMsgs ++ zip nodes (repeat msg))
    runSession (Or session session') stdGen' sentMsgs =
      let (bool, stdGen'') = random stdGen'
      in  if bool
            then runSession session stdGen'' sentMsgs
            else runSession session' stdGen'' sentMsgs
    runSession (End state') _stdGen' sentMsgs = (mock {state = Right state'}, sentMsgs)

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

type role SomeMock representational representational
data SomeMock node msg = forall state. SomeMock (Mock state node msg)

type role Sim nominal representational
data Sim node msg = Sim
  { nodes :: Map node (SomeMock node msg)
  , messages :: [Envelope node msg]
  , prng :: StdGen
  , trace :: [Envelope node msg]
  }

type role Envelope representational representational
data Envelope node msg = Envelope
  { from :: node
  , to :: node
  , content :: msg
  -- , arrivalTime :: UTCTime
  }
  deriving (Eq, Show)

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
  (Ord node, Show node) =>
  Sim node msg
  -> Either (Sim node msg) (Sim node msg)
stepSim sim = case sim.messages of
  [] -> Right sim
  envelope : envelopes -> case sim.nodes Map.! envelope.to of
    SomeMock mock ->
      let (stdGen', stdGen'') = split sim.prng
          (mock', sentMsgs) = runMock mock envelope stdGen'
      in  Left
            Sim
              { nodes = Map.insert envelope.to (SomeMock mock') sim.nodes
              , messages = envelopes ++ map (uncurry (Envelope envelope.to)) sentMsgs
              , prng = stdGen''
              , trace = envelope : sim.trace
              }

runSim ::
  (Ord node, Show node) => Sim node msg -> [Envelope node msg]
runSim sim = case stepSim sim of
  Right sim' -> reverse sim'.trace
  Left sim' -> runSim sim'

test :: IO ()
test = do
  let seed = 123
  sim <-
    newSim
      twoPCDeployment
      [Envelope {from = Client, to = TM, content = Write}]
      seed
  -- mapM_ print (runSim sim)
  assert (runSim sim == unhappy) (return ())
  where
    trace =
      [ Envelope {from = Client, to = TM, content = Write}
      , Envelope {from = TM, to = RM1, content = Prepare}
      , Envelope {from = TM, to = RM2, content = Prepare}
      , Envelope {from = RM1, to = TM, content = Prepared}
      , Envelope {from = RM2, to = TM, content = Prepared}
      , Envelope {from = TM, to = RM1, content = Commit}
      , Envelope {from = TM, to = RM2, content = Commit}
      , Envelope {from = RM1, to = TM, content = Ack}
      , Envelope {from = RM2, to = TM, content = Ack}
      , Envelope {from = TM, to = Client, content = Ack}
      ]

    unhappy =
      [ Envelope {from = Client, to = TM, content = Write}
      , Envelope {from = TM, to = RM1, content = Prepare}
      , Envelope {from = TM, to = RM2, content = Prepare}
      , Envelope {from = RM1, to = TM, content = Aborted}
      , Envelope {from = RM2, to = TM, content = Aborted}
      , Envelope {from = TM, to = RM1, content = Abort}
      , Envelope {from = TM, to = RM2, content = Abort}
      , Envelope {from = TM, to = Client, content = Abort}
      ]
