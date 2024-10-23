{-# LANGUAGE TemplateHaskell #-}

module Spex.CommandLine.ArgParser where

import Data.Version (showVersion)
import GitHash (getGitInfo, giDirty, giHash)
import Options.Applicative
import System.Environment (lookupEnv)

import Paths_spex (version)

------------------------------------------------------------------------

data CmdLineArgs = CmdLineArgs
  { host           :: String
  , port           :: Int
  , health         :: String
  , reset          :: String
  , numTests       :: Maybe Word
  , seed           :: Maybe Int
  , logging        :: Logging
  , nonInteractive :: Bool
  , specFilePath   :: FilePath
  }

data Logging = Quiet Bool | Verbose Bool | VeryVerbose Bool

parseCmdLineArgs :: IO CmdLineArgs
parseCmdLineArgs = do
  git <- getGitInfo "." >>= \case
           Right gi -> return $ giHash gi ++ if giDirty gi
                                             then "-dirty"
                                             else ""
           Left _e  -> lookupEnv "SPEX_GIT_HASH" >>= \case
                         Nothing  -> return "UNKNOWN"
                         Just git -> return git
  execParser (opts git)
  where
    opts git = info (cmdLineArgs <**> simpleVersioner versionHash <**> helper)
      ( fullDesc
      <> progDesc "The Spex specification language."
      <> header "spex - specification language"
      )
      where
        versionHash = concat [ "v", showVersion version, " ", git ]

cmdLineArgs :: Parser CmdLineArgs
cmdLineArgs = CmdLineArgs
  <$> strOption
        (  long "host"
        <> metavar "URL"
        <> help "Host"
        <> value "http://localhost"
        )
  <*> option auto
        (  long "port"
        <> short 'p'
        <> metavar "PORT"
        <> help "Port"
        <> value 8080
        )
  <*> strOption
        (  long "health"
        <> metavar "PATH"
        <> help "Health check endpoint"
        <> value "/health"
        )
  <*> strOption
        (  long "reset"
        <> metavar "PATH"
        <> help "Reset"
        <> value "/_reset"
        )
  <*> optional (option auto
        (  long "tests"
        <> help "Number of tests to run"
        <> metavar "INT"
        ))
  <*> optional (option auto
        (  long "seed"
        <> help "Seed for pseudo-random number generator"
        <> metavar "INT"
        ))
  <*> (fmap Verbose (switch
        (  long "verbose"
        <> help "Enable more verbose logging"
        )) <|>
       fmap VeryVerbose (switch
        (  long "very-verbose"
        <> help "Enable even more verbose logging"
        )) <|>
       fmap Quiet (switch
        (  long "quiet"
        <> help "Enable more quiet logging"
        )))
  <*> switch
        (  long "non-interactive"
        <> help "Disable fancy logging"
        )
  <*> strArgument
        (  metavar "FILE"
        <> help "Input file"
        )


