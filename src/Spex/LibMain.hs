module Spex.LibMain where

import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
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
import Spex.Verifier.Generator
import Spex.Verifier.HealthChecker

------------------------------------------------------------------------

libMain :: IO ()
libMain = do
  setLocaleEncoding utf8
  opts <- parseCliOptions
  mainWith opts
  exitSuccess

mainWith :: Options -> IO ()
mainWith (optsCommand -> Repl {}) = notSupportedYet
mainWith (optsCommand -> Import {}) = notSupportedYet
mainWith (optsCommand -> Export {}) = notSupportedYet
mainWith opts = do
  appEnv <- newAppEnv opts
  let (app, specFile) = case opts.optsCommand of
        Verify vopts -> (verifyApp vopts, vopts.specFilePath)
        Format fopts -> (formatApp fopts, fopts.specFilePath)
        Check copts -> (checkApp copts, copts.specFilePath)
        _ -> error "impossible"
  runApp appEnv (app >> flushLogger >> closeLogger) >>= \case
    Left err -> do
      lbs <- LBS.readFile specFile
      Right () <- runApp appEnv (logError (displayAppError specFile lbs err))
      exitFailure
    Right () -> return ()

notSupportedYet :: IO ()
notSupportedYet = do
  putStrLn "Not supported yet!"
  exitFailure

verifyApp :: VerifyOptions -> App ()
verifyApp opts = do
  let deploy =
        Deployment
          (HostPort opts.host opts.port)
          (HealthCheckPath opts.health)
          (ResetPath opts.reset)
  info_ ""
  info $
    "Verifying the deployment:    "
      <> displayDeployment deploy
      <> "\n"
      <> "  against the specification:   "
      <> opts.specFilePath
      <> "\n"
  bs <- liftIO (try (BS.readFile opts.specFilePath)) <?> ReadSpecFileError
  info $ "Checking the specification.\n"
  spec <- pure (runParser specP bs) <?> ParserError
  scopeCheck spec
  info "Waiting for health check to pass...\n"
  healthChecker deploy
  done "Health check passed!\n"
  info $ "Starting to run tests...\n"
  (prng, seed) <- liftIO (newPrng opts.seed)
  result <- verify opts spec deploy prng
  done $
    "Done testing, here are the results: \n\n"
      <> displayResult spec result seed

formatApp :: FormatOptions -> App ()
formatApp opts = do
  bs <- liftIO (try (BS.readFile opts.specFilePath)) <?> ReadSpecFileError
  spec <- pure (runParser specP bs) <?> ParserError
  liftIO (putSpec spec)

checkApp :: CheckOptions -> App ()
checkApp opts = do
  bs <- liftIO (try (BS.readFile opts.specFilePath)) <?> ReadSpecFileError
  spec <- pure (runParser specP bs) <?> ParserError
  scopeCheck spec
  done "Specification is well-formed!"

------------------------------------------------------------------------

testMain :: [String] -> IO ()
testMain args = do
  opts <- parseCliOptions_ args
  mainWith opts
