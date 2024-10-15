module Spex.CommandLine.ArgParser where

import Options.Applicative

------------------------------------------------------------------------

data CmdLineArgs = CmdLineArgs
  { specFilePath :: FilePath
  , host         :: String
  , port         :: Int
  , health       :: String
  , reset        :: String
  , numTests     :: Maybe Word
  , seed         :: Maybe Int
  }

parseCmdLineArgs :: IO CmdLineArgs
parseCmdLineArgs = execParser opts
  where
    opts = info (cmdLineArgs <**> helper)
      ( fullDesc
      <> progDesc "The Spex specification language."
      <> header "spex - specification language"
      )

cmdLineArgs :: Parser CmdLineArgs
cmdLineArgs = CmdLineArgs
  <$> strOption
        (  long "file"
        <> short 'f'
        <> metavar "FILE"
        <> help "Input file"
        )
  <*> strOption
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

