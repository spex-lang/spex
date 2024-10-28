{-# LANGUAGE OverloadedStrings #-}

module Spex.Syntax where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BS8
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

data TypeDecl = TypeDecl { id :: TypeId, rhs :: Type }
  deriving Show

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
displayDeployment d = BS8.unpack d.hostPort.host <> ":" <> show d.hostPort.port

data HostPort = HostPort { host :: ByteString, port :: Int }
  deriving Show

data HealthCheck = HealthCheckPath ByteString | HealthCheckScript FilePath
  deriving Show

data Reset = ResetPath ByteString | ResetScript FilePath
  deriving Show
