{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Spex.CommandLine.Option where

-- Here's a nice introduction to the optparse-applicative library:
--   https://www.prborges.com/2023/introduction-to-optparse-applicative/
-- Another useful references:
--   https://github.com/Gabriella439/grace/blob/main/src/Grace.hs
--   https://github.com/pcapriotti/optparse-applicative?tab=readme-ov-file#commands

import Data.ByteString (ByteString)
import Data.Version (showVersion)
import Options.Applicative

import Paths_spex (version)
import Spex.CommandLine.GitHash (tGitHash)

------------------------------------------------------------------------

data Options = Options
  { optsCommand :: Command
  , logging :: Logging
  , logFile :: Maybe FilePath
  , nonInteractive :: Bool
  }

data Logging = Quiet Bool | Verbose Bool | Trace Bool

data Command
  = Verify VerifyOptions
  | Format FormatOptions
  | Check CheckOptions
  | Mock MockOptions
  | Repl ReplOptions
  | Import ImportOptions
  | Export ExportOptions

data VerifyOptions = VerifyOptions
  { host :: ByteString
  , port :: Int
  , health :: ByteString
  , reset :: ByteString
  , numTests :: Word
  , seed :: Maybe Int
  , noShrinking :: Bool
  , specFilePath :: FilePath
  }

data FormatOptions = FormatOptions
  { specFilePath :: FilePath
  , output :: Maybe FilePath
  -- , inPlace :: Inplace ; data InPlace = NotInplace | Inplace | InplaceWithBackup Suffix
  }

data CheckOptions = CheckOptions
  {specFilePath :: FilePath}

data MockOptions = MockOptions
  { specFilePath :: FilePath
  , port :: Int
  , seed :: Maybe Int
  }

data ReplOptions = ReplOptions
data ImportOptions = ImportOptions
data ExportOptions = ExportOptions

------------------------------------------------------------------------

parseCliOptions :: IO Options
parseCliOptions = execParser parserInfo

parseCliOptions_ :: [String] -> IO Options
parseCliOptions_ args =
  handleParseResult (execParserPure defaultPrefs parserInfo args)

parserInfo :: ParserInfo Options
parserInfo =
  info
    (helper <*> simpleVersioner versionHash <*> parser)
    ( progDesc "Command-line utility for the Spex language"
        <> header "spex - specification language and verifier"
        <> footer "See https://spex-lang.org for more documentation."
    )
  where
    versionHash = concat ["v", showVersion version, " ", ($(tGitHash))]

parser :: Parser Options
parser =
  Options
    <$> hsubparser
      ( command
          "verify"
          ( info
              (Verify <$> verify)
              (progDesc "Verify a deployment against a Spex specification")
          )
          <> command
            "format"
            ( info
                (Format <$> format)
                (progDesc "Format a Spex specification")
            )
          <> command
            "check"
            ( info
                (Check <$> check)
                (progDesc "Check a Spex specification for internal consistency")
            )
          <> command
            "mock"
            ( info
                (Mock <$> mock)
                (progDesc "Mock a Spex specification")
            )
          <> command
            "repl"
            ( info
                (Repl <$> repl)
                (progDesc "Enter a REPL for a Spex specification against a deployment")
            )
          <> command
            "import"
            ( info
                (Import <$> import')
                ( progDesc
                    "Convert another specification format into a Spex specification"
                )
            )
          <> command
            "export"
            ( info
                (Export <$> export)
                ( progDesc
                    "Convert a Spex specification into another specification format"
                )
            )
      )
    <*> ( fmap
            Verbose
            ( switch
                ( long "verbose"
                    <> help "Enable more verbose logging"
                )
            )
            <|> fmap
              Trace
              ( switch
                  ( long "trace"
                      <> help "Enable even more verbose logging"
                  )
              )
            <|> fmap
              Quiet
              ( switch
                  ( long "quiet"
                      <> help "Enable more quiet logging"
                  )
              )
        )
    <*> optional
      ( strOption
          ( long "log-file" <> metavar "FILE" <> help "File to append log output to"
          )
      )
    <*> switch
      ( long "non-interactive"
          <> help "Disable fancy logging"
      )
  where
    verify :: Parser VerifyOptions
    verify =
      VerifyOptions
        <$> strOption
          ( long "host"
              <> metavar "URL"
              <> help "Host"
              <> value "http://localhost"
          )
        <*> option
          auto
          ( long "port"
              <> short 'p'
              <> metavar "PORT"
              <> help "Port"
              <> value 8080
          )
        <*> strOption
          ( long "health"
              <> metavar "PATH"
              <> help "Health check endpoint"
              <> value "/health"
          )
        <*> strOption
          ( long "reset"
              <> metavar "PATH"
              <> help "Reset"
              <> value "/_reset"
          )
        <*> option
          auto
          ( long "tests"
              <> help "Number of tests to run"
              <> metavar "INT"
              <> value 100
          )
        <*> optional
          ( option
              auto
              ( long "seed"
                  <> help "Seed for pseudo-random number generator"
                  <> metavar "INT"
              )
          )
        <*> switch
          ( long "no-shrinking"
              <> help "Disable shrinking"
          )
        <*> specFile

    specFile :: Parser FilePath
    specFile =
      strArgument
        ( metavar "FILE"
            <> help "Input Spex specification file"
        )

    format :: Parser FormatOptions
    format =
      FormatOptions
        <$> specFile
        <*> optional
          ( strOption
              ( long "output"
                  <> short 'o'
                  <> metavar "FILE"
                  <> help "Output file path for the formatted specification"
              )
          )

    check :: Parser CheckOptions
    check = CheckOptions <$> specFile

    mock :: Parser MockOptions
    mock =
      MockOptions
        <$> specFile
        <*> option
          auto
          ( long "port"
              <> short 'p'
              <> metavar "PORT"
              <> help "Port"
              <> value 8080
          )
        <*> optional
          ( option
              auto
              ( long "seed"
                  <> help "Seed for pseudo-random number generator"
                  <> metavar "INT"
              )
          )

    repl :: Parser ReplOptions
    repl = pure ReplOptions

    import' :: Parser ImportOptions
    import' = pure ImportOptions

    export :: Parser ExportOptions
    export = pure ExportOptions
