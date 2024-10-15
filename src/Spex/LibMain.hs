module Spex.LibMain where

import Control.Exception
import qualified Data.ByteString as BS
import Data.Maybe
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
  putStrLn ""
  appEnv <- newAppEnv
  runApp appEnv (app args) >>= \case
    Left err -> do
      Right () <- runApp appEnv (logError (displayAppError err))
      exitFailure
    Right () -> exitSuccess

app :: CmdLineArgs -> App ()
app args = do
  info $ "Verifying the deployment:    " <> args.host <> ":" <> show args.port <> "\n" <>
         "  against the specification:   " <> args.specFilePath <> "\n"--  <> BS.unpack spec.component.id
  let config = Config (fromMaybe 100 args.numTests) args.seed
      deploy = Deployment (HostPort args.host args.port)
                 (HealthCheckPath args.health) (ResetPath args.reset)
  bs <- liftIO (try (BS.readFile args.specFilePath))
          <?> ReadSpecFileError args.specFilePath
  info $ "Parsing the specification.\n"
  spec <- pure (runParser specP bs) <?> ParserError
  info $ "Waiting for health check to pass.\n"
  -- XXX:
  info $ "Starting to run tests.\n"
  verify config spec deploy
  info $ "All tests passed!"
