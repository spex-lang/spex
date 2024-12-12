module Spex.Experiment.Protocol3 (module Spex.Experiment.Protocol3) where

import Control.Concurrent.STM
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Typeable
import System.Random
import Text.Read (readEither)

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
  = Send node msg (Session state node msg)
  | Recv node (msg -> Session state node msg)
  | Or (Session state node msg) (Session state node msg)
  | End state

data SomeProtocol node msg
  = forall state.
    SomeProtocol (Protocol state node msg) state

data TMState = TMInit | TMWorking | TMCommitted
  deriving (Eq, Show)

data RMState = RMInit | RMPrepared
  deriving (Eq, Show)

data ClientState = ClientInit
  deriving (Eq, Show)

data Msg = Write | Prepare | Prepared | Commit | Comitted | Ack
  deriving (Show)

tmProtocol :: Protocol TMState Node Msg
tmProtocol =
  Protocol
    [ Transition
        (== TMInit)
        ( Recv
            Client
            (\Write -> Send RM1 Prepare (Send RM2 Prepare (End TMWorking)))
        )
    , Transition
        (== TMWorking)
        ( Recv
            RM1
            ( \Prepared ->
                Recv
                  RM2
                  (\Prepared -> Send RM1 Commit (Send RM2 Commit (End TMCommitted)))
            )
        )
    , Transition
        (== TMCommitted)
        ( Recv RM1 (\Ack -> Recv RM2 (\Ack -> Send Client Ack (End TMInit)))
        )
    ]

-- , Rule
--    (== Working)
--    (\_s -> Recv RM1 (\Abort -> Send RM2 Abort (End Aborted)))
-- , -- XXX: symmetric...
--  Rule
--    (== Working)
--    (\_s -> Recv RM2 (\Abort -> Send RM1 Abort (End Aborted)))
-- , -- XXX: Symmetric to the above
--  Rule
--    (== Working)
--    ( \_s ->
--        Recv
--          RM2
--          ( \Prepare ->
--              Recv
--                RM1
--                (\Prepare -> Send RM1 Commit (Send RM2 Commit (End Committed)))
--          )
--    )
-- , Rule
--    (== Committed)
--    ( \_s -> Recv RM1 (\Ack -> Recv RM2 (\Ack -> Send Client Ack (End Init)))
--    )
-- , -- XXX: symmetric
--  Rule
--    (== Committed)
--    ( \_s -> Recv RM2 (\Ack -> Recv RM1 (\Ack -> Send Client Ack (End Init)))
--    )

rmProtocol :: Protocol RMState Node Msg
rmProtocol =
  Protocol
    [ Transition
        (== RMInit)
        ( Recv
            TM
            ( \Prepare ->
                Send TM Prepared (End RMPrepared)
                -- `Or` Send TM Abort (End Aborted)
            )
        )
    , Transition
        (== RMPrepared)
        (Recv TM (\Commit -> Send TM Ack (End RMInit)))
    ]

-- , Rule (== Prepared) (\_s -> Recv TM (\Abort -> End Aborted))
-- , Rule (== Aborted) (\_s -> Recv TM (\Abort -> End Aborted))

clientProtocol :: Protocol ClientState Node Msg
clientProtocol =
  Protocol [Transition (== ClientInit) (Recv TM (\Ack -> End ClientInit))]

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
  (Eq node) =>
  Mock state node msg
  -> Envelope node msg
  -> (Mock state node msg, [(node, msg)])
runMock mock envelope = case mock.state of
  Left (Recv node k)
    | node /= envelope.from -> error "runMock: node mismatch"
    | otherwise -> go [] (k envelope.content)
  Right state -> case lookupState state mock.protocol of
    [] -> (mock, [])
    [Recv node k]
      | node /= envelope.from -> error "runMock: node mismatch"
      | otherwise -> go [] (k envelope.content)
    [Send node msg session'] -> go [(node, msg)] session'
    _ : _ : _ -> error "runMock: more than one session"
  where
    go sentMsgs (Send toNode msg session') = go ((toNode, msg) : sentMsgs) session'
    go sentMsgs (Or session session') = error "XXX: or"
    go sentMsgs (End state') = (mock {state = Right state'}, reverse sentMsgs)
    go sentMsgs session = (mock {state = Left session}, reverse sentMsgs)

lookupState ::
  state
  -> Protocol state node msg
  -> [Session state node msg]
lookupState state (Protocol transitions) = go transitions
  where
    go [] = []
    go (Transition pred session : transitions')
      | pred state = session : go transitions'
      | otherwise = go transitions'

data SomeMock node msg = forall state. SomeMock (Mock state node msg)

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
  deriving (Show)

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
  (Ord node) => Sim node msg -> Either (Sim node msg) (Sim node msg)
stepSim sim = case sim.messages of
  [] -> Right sim
  envelope : envelopes -> case sim.nodes Map.! envelope.to of
    SomeMock mock ->
      let (mock', sentMsgs) = runMock mock envelope
      in  Left
            Sim
              { nodes = Map.insert envelope.to (SomeMock mock') sim.nodes
              , messages = envelopes ++ map (uncurry (Envelope envelope.to)) sentMsgs
              , prng = sim.prng
              , trace = envelope : sim.trace
              }

runSim ::
  (Ord node) => Sim node msg -> [Envelope node msg]
runSim sim = case stepSim sim of
  Right sim' -> reverse sim'.trace
  Left sim' -> runSim sim'

test :: IO ()
test = do
  sim <-
    newSim
      twoPCDeployment
      [Envelope {from = Client, to = TM, content = Write}]
      123
  mapM_ print (runSim sim)
