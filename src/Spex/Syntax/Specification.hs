{-# LANGUAGE OverloadedStrings #-}

module Spex.Syntax.Specification where

import Data.String (IsString)
import Data.Text (Text)

import Spex.Syntax.Operation
import Spex.Syntax.Position
import Spex.Syntax.Type

------------------------------------------------------------------------

data Spec = Spec
  { component :: Component
  }
  deriving (Show)

data Component = Component
  { id :: ComponentId
  , typeDecls :: [TypeDecl]
  , opDecls :: [Ann OpDecl]
  }
  deriving (Show)

newtype ComponentId = ComponentId Text
  deriving (Show, IsString)

data TypeDecl = TypeDecl {typeId :: TypeId, rhs :: Type}
  deriving (Show)

data Deployment = Deployment
  -- { repo        :: ByteString
  -- , hash        :: ByteString
  -- semanticVersion?
  -- ssh?
  { hostPort :: HostPort
  , healthCheck :: HealthCheck
  , reset :: Reset
  }
  deriving (Show)

data HostPort = HostPort {host :: Text, port :: Int}
  deriving (Show)

data HealthCheck = HealthCheckPath Text | HealthCheckScript FilePath
  deriving (Show)

data Reset = ResetPath Text | ResetScript FilePath
  deriving (Show)
