module Spex.LibMain where

import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import GHC.IO.Encoding (setLocaleEncoding)
import System.Exit
import System.IO (utf8)

import Spex.CommandLine.Option
import Spex.Lexer
import Spex.Monad
import Spex.Parser
import Spex.PrettyPrinter
import Spex.Syntax
import Spex.TypeChecker
import Spex.Verifier
import Spex.Verifier.HealthChecker

------------------------------------------------------------------------

libMain :: IO ()
libMain = do
  setLocaleEncoding utf8
  opts <- parseCliOptions
  mainWith opts

mainWith :: Options -> IO ()
mainWith opts = do
  appEnv <- newAppEnv opts
  let (app, specFile) = case opts.optsCommand of
                          Verify vopts -> (verifyApp vopts, vopts.specFilePath)
                          Format fopts -> (formatApp fopts, fopts.specFilePath)
                          _ -> error "Not supported yet!"
  runApp appEnv app >>= \case
    Left err -> do
      lbs <- LBS.readFile specFile
      Right () <- runApp appEnv (logError (displayAppError specFile lbs err))
      exitFailure
    Right () -> exitSuccess

verifyApp :: VerifyOptions -> App ()
verifyApp opts = do
  let deploy  = Deployment (HostPort opts.host opts.port)
                   (HealthCheckPath opts.health) (ResetPath opts.reset)
  info_ ""
  info $ "Verifying the deployment:    " <> displayDeployment deploy <> "\n" <>
         "  against the specification:   " <> opts.specFilePath <> "\n"
  bs <- liftIO (try (BS.readFile opts.specFilePath)) <?> ReadSpecFileError
  info $ "Checking the specification.\n"
  spec <- pure (runParser specP bs) <?> ParserError
  scopeCheck spec
  info $ "Waiting for health check to pass.\n"
  healthChecker deploy
  info $ "Starting to run tests.\n"
  result <- verify opts spec deploy

  info $ "All tests passed, here are the results: \n\n" <> displayResult result

formatApp :: FormatOptions -> App ()
formatApp opts = do
  bs <- liftIO (try (BS.readFile opts.specFilePath)) <?> ReadSpecFileError
  spec <- pure (runParser specP bs) <?> ParserError
  liftIO (putSpec spec)

------------------------------------------------------------------------

testMain :: [String] -> IO ()
testMain args = do
  opts <- parseCliOptions_ args
  mainWith opts
