module Spex.LibMain where

import Control.Exception
import qualified Data.ByteString as BS
import System.Exit

import Spex.CommandLine.ArgParser
import Spex.Lexer
import Spex.Monad
import Spex.Parser
import Spex.Syntax
import Spex.Verifier

------------------------------------------------------------------------

libMain :: IO ()
libMain = do
  args <- parseCmdLineArgs
  appEnv <- newAppEnv args
  runApp appEnv app >>= \case
    Left err -> do
      Right () <- runApp appEnv (logError (displayAppError appEnv.specFile err))
      exitFailure
    Right () -> exitSuccess

app :: App ()
app = do
  deploy <- asks deployment
  specFile   <- asks specFile
  info $ "Verifying the deployment:    " <> displayDeployment deploy <> "\n" <>
         "  against the specification:   " <> specFile <> "\n"--  <> BS.unpack spec.component.id
  bs <- liftIO (try (BS.readFile specFile)) <?> ReadSpecFileError
  info $ "Parsing the specification.\n"
  spec <- pure (runParser specP bs) <?> ParserError
  info $ "Waiting for health check to pass.\n"
  -- XXX:
  info $ "Starting to run tests.\n"
  config <- asks config
  result <- verify config spec deploy
  info $ "All tests passed, here are the results: \n\n" <> displayResult result
