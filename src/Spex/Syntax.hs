{-# LANGUAGE OverloadedStrings #-}

module Spex.Syntax where

import Data.ByteString (ByteString)
import Data.String (IsString)

import Spex.Syntax.Operation
import Spex.Syntax.Type

------------------------------------------------------------------------

data Spec = Spec
  { component  :: Component
  }
  deriving Show

data Component = Component
  { id        :: ComponentId
  , typeDecls :: [TypeDecl]
  , opDecls   :: [OpDecl]
  }
  deriving Show

newtype ComponentId = ComponentId ByteString
  deriving (Show, IsString)

data TypeDecl = TypeDecl TypeId Type
  deriving Show

newtype TypeId = TypeId ByteString
  deriving (Show, IsString)

data Deployment = Deployment
  -- { repo        :: ByteString
  -- , hash        :: ByteString
  -- semanticVersion?
  -- ssh?
  { hostPort    :: HostPort
  , healthCheck :: HealthCheck
  , reset       :: Reset
  }
  deriving Show

displayDeployment :: Deployment -> String
displayDeployment d = d.hostPort.host <> ":" <> show d.hostPort.port

data HostPort = HostPort { host :: String, port :: Int }
  deriving Show

data HealthCheck = HealthCheckPath String | HealthCheckScript FilePath
  deriving Show

data Reset = ResetPath String | ResetScript FilePath
  deriving Show
