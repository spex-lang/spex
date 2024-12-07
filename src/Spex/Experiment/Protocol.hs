{-# LANGUAGE OverloadedStrings #-}
 
module Spex.Experiment.Protocol (module Spex.Experiment.Protocol) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Spex.Generator.Combinator
import Spex.Syntax.Position
import Spex.Syntax.Type

------------------------------------------------------------------------

type OpId = String

newtype Protocol state = Protocol (Map state [(OpId, Type, Type, state)])

filePathT :: Type
filePathT = StringT

handleT :: Type
handleT = UserT (Ann (Pos 0) "Handle")

errorT :: Type
errorT = IntT

data FileState = Closed | Opened
  deriving (Eq, Ord, Show)

posixFileSystemProtocol :: Protocol FileState
posixFileSystemProtocol =
  Protocol $
    Map.fromList
      [
        ( Closed
        ,
          [ ("open", filePathT, handleT, Opened)
          , ("open", filePathT, errorT, Closed)
          ]
        )
      ,
        ( Opened
        ,
          [ ("close", handleT, UnitT, Closed)
          , ("close", handleT, errorT, Closed)
          ]
        )
      ,
        ( Opened
        ,
          [ ("read", ProdT handleT IntT, StringT, Opened)
          , ("read", ProdT handleT IntT, errorT, Closed)
          ]
        )
      ]

genTrace :: (Ord state) => Protocol state -> state -> Gen [OpId]
genTrace (Protocol transitions) state0 = sized (go state0 [])
  where
    go _state ops 0 = return (reverse ops)
    go state ops size = case Map.lookup state transitions of
      Nothing -> return (reverse ops)
      Just [] -> return (reverse ops)
      Just options -> do
        (op, _ty, _ty', state') <- elements options
        go state' (op : ops) (size - 1)

------------------------------------------------------------------------

data Patterns state
  = RequestResponse Type Type state state -- !T1 ?T2 & s1 | Timeout & s2
  | FireAndForget Type state -- !T & s
  | Respond Type Type state -- ?T1 !T2 & s
  | ReceiveMany Type state state -- ?T+ & s1 | Timeout & s2
  | ReceiveTimeoutSend Type state Type state -- ?T1 & s1 | ?Timeout !T2 & s2

data ProtocolState state
  = InState state
  | InSessionType (SessionType state)

data SessionType state
  = Send Type (ProtocolState state)
  | Recv Type (ProtocolState state) state

reqResp t1 t2 s1 s2 = Send t1 (InSessionType (Recv t2 s1 s2))
fire t s = Send t (InState s)
resp t1 t2 s = Recv t1 (InSessionType (fire t2 s))

data SessionProtocol state
  = SessionProtocol (Map state [SessionType state])

{-
follower & ping : !Ping ?Pong & follower
	      	        |
                        ?Timeout & leaderless

leaderless & election : !Election & election

follower & ?Election !Ok & leaderless

election & ok : ?Ok+ & follower
              | Timeout & won

won & announce : !Coordinator & leader

-- or, we can combine the last two rules as such:

election & ok : ?Ok+ & follower
              | ?Timeout !Coordinator & leader
-}
data BullyState = Follower | Candidate | Leader | Election | Won
  deriving (Eq, Ord, Show)

userT :: TypeId -> Type
userT tid = UserT (Ann (Pos 0) tid)

bullyAlgo :: SessionProtocol BullyState
bullyAlgo =
  SessionProtocol $
    Map.fromList
      [ (Candidate, [Send (userT "Election") (InState Election)])
      ,
        ( Follower
        , [Send UnitT (InSessionType (Recv UnitT (InState Follower) Candidate))]
        )
      ,
        ( Follower
        ,
          [ Recv
              (userT "Election")
              (InSessionType (Send (userT "Ok") (InState Candidate)))
              Follower -- NOTE: Timeout is not noticed here...
          ]
        )
      , (Election, [Recv (userT "Ok") (InState Follower) Won])
      , (Won, [Send (userT "Coorinator") (InState Leader)])
      ]

data Action = RecvA Type | SendA Type | TimeoutA Type
  deriving (Show)

data ContractError state
  = NothingAllowedInState state Action
  | NotAllowedInState state Action
  deriving (Show)

checkState ::
  (Ord state) =>
  SessionProtocol state
  -> state
  -> Action
  -> Maybe (ProtocolState state)
checkState (SessionProtocol transitions) state action = do
  options <- Map.lookup state transitions
  go options
  where
    go [] = Nothing
    go (Recv ty k state : rest) = case action of
      RecvA ty' | ty == ty' -> Just k
      TimeoutA ty' | ty == ty' -> Just (InState state)
      _otherwise -> go rest
    go (Send ty k : rest) = case action of
      SendA ty' | ty == ty' -> Just k
      _otherwise -> go rest

checkSession ::
  SessionType state
  -> Action
  -> Either (ContractError state) (ProtocolState state)
checkSession (Send ty k) (SendA ty')
  | ty == ty' = return k
  | otherwise = Left undefined
checkSession (Recv ty k _state) (RecvA ty')
  | ty == ty' = return k
  | otherwise = Left undefined
checkSession (Recv ty _k state) (TimeoutA ty')
  | ty == ty' = return (InState state)
  | otherwise = Left undefined
checkSession _ _ = Left undefined

check ::
  (Ord state) =>
  SessionProtocol state
  -> ProtocolState state
  -> [Action]
  -> Either (ContractError state) state
check _sessionProtocol (InState state) [] = Right state
check sessionProtocol (InState state) (action : actions) =
  case checkState sessionProtocol state action of
    Nothing -> Left undefined
    Just k -> check sessionProtocol k actions
check _sessionProtocol (InSessionType sty) [] = Left undefined
check sessionProtocol (InSessionType sty) (action : actions) = do
  eResult <- checkSession sty action
  check sessionProtocol eResult actions

unit_bullyAlgo = check bullyAlgo (InState Follower) [SendA UnitT, RecvA UnitT]

{-
mockNode :: SessionProtocol state -> state -> Type -> (state, [Type])
mockNode (SessionProtocol transitions) state0 input = case Map.lookup state0 transitions of
  Nothing -> error ""
  Just options -> go options
    where
      go [] = error ""
      go ((opId, Recv ty sty) : rest) | ty == input = case sty of
        Send ty' sty' -> undefined
        End s -> (s, [])
        EndOrTimeout s1 s2 -> undefined
        _ -> error ""
 -}
