module Spex.Experiment.Protocol3 (module Spex.Experiment.Protocol3) where

import Control.Concurrent.STM
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

------------------------------------------------------------------------

type Topology node = Map node [node]

data Node = TM | RM1 | RM2
  deriving (Eq, Ord, Show)

twoPCTopology :: Topology Node
twoPCTopology =
  Map.fromList
    [ (TM, [RM1, RM2])
    , (RM1, [TM])
    , (RM2, [TM])
    ]

data Protocol state node msg = Protocol [Transition state node msg]

data Transition state node msg = Transition state (Session state node msg)

data Session state node msg
  = Request msg (Session state node msg)
  | Send node msg (Session state node msg)
  | Recv node (msg -> Session state node msg)
  | Or (Session state node msg) (Session state node msg)
  | End state

data SomeProtocol node
  = forall state msg. SomeProtocol (Protocol state node msg)

data TMState = TMInit | TMWorking | TMCommitted
data RMState = RMInit

data Msg = Write | Prepare

tmProtocol :: Protocol TMState Node Msg
tmProtocol =
  Protocol
    [ Transition
        TMInit
        (Request Write (Send RM1 Prepare (Send RM2 Prepare (End TMWorking))))
    ]

rmProtocol :: Protocol RMState Node Msg
rmProtocol = undefined

data Deployment node = Deployment
  { topology :: Topology node
  , protocols :: Map node (SomeProtocol node)
  , server :: node
  }

twoPCDeployment :: Deployment Node
twoPCDeployment =
  Deployment
    { topology = twoPCTopology
    , protocols =
        Map.fromList
          [ (TM, SomeProtocol tmProtocol)
          , (RM1, SomeProtocol rmProtocol)
          , (RM2, SomeProtocol rmProtocol)
          ]
    , server = TM
    }

data Channels node msg = Channels
  { incoming :: Map node (TQueue msg)
  , outgoing :: Map node (TQueue msg)
  }

data Mock state node msg = Mock
  { channels :: Channels node msg
  , state :: IORef (Either (Session state node msg) state)
  }

data SomeMock node

data ClientChannel msg

deploy ::
  Deployment node -> IO ([(node, SomeMock node)], ClientChannel msg)
deploy = undefined
