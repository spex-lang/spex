module Spex.LibMain where

import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import GHC.IO.Encoding (setLocaleEncoding)
import Spex.Verifier.Result
import System.Exit
import System.IO (IOMode (WriteMode), utf8, withFile)

import Spex.CommandLine.Option
import Spex.Generator.Prng
import Spex.Lexer
import Spex.Mock
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
  mainWith opts notTesting
  exitSuccess

type Testing = Bool

notTesting :: Testing
notTesting = False

mainWith :: Options -> Testing -> IO ()
mainWith (optsCommand -> Repl {}) _testing = notSupportedYet
mainWith (optsCommand -> Import {}) _testing = notSupportedYet
mainWith (optsCommand -> Export {}) _testing = notSupportedYet
mainWith opts testing = do
  appEnv <- newAppEnv opts
  let (app, specFile) = case opts.optsCommand of
        Verify vopts
          -- During testing, redirect the printing of the result to the logs,
          -- so it's easier to test the complete output using one golden test.
          | testing -> (verifyAppLog vopts, vopts.specFilePath)
          | otherwise -> (verifyAppStdout vopts, vopts.specFilePath)
        Format fopts -> (formatApp fopts, fopts.specFilePath)
        Mock mopts -> (mockApp mopts, mopts.specFilePath)
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

verifyAppStdout :: VerifyOptions -> App ()
verifyAppStdout opts =
  verifyApp
    opts
    (\spec result seed -> liftIO (putResult spec result seed))

verifyAppLog :: VerifyOptions -> App ()
verifyAppLog opts =
  verifyApp
    opts
    (\spec result seed -> info_ (displayResult spec result seed))

verifyApp ::
  VerifyOptions -> (Spec -> Result -> Int -> App ()) -> App ()
verifyApp opts handleResult = do
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
  done $ "Done testing!"
  handleResult spec result seed

formatApp :: FormatOptions -> App ()
formatApp opts = do
  bs <- liftIO (try (BS.readFile opts.specFilePath)) <?> ReadSpecFileError
  spec <- pure (runParser specP bs) <?> ParserError
  case opts.output of
    Nothing -> liftIO (putSpec spec)
    Just fp -> liftIO $ withFile fp WriteMode $ \h -> hPutSpec h spec

checkApp :: CheckOptions -> App ()
checkApp opts = do
  bs <- liftIO (try (BS.readFile opts.specFilePath)) <?> ReadSpecFileError
  spec <- pure (runParser specP bs) <?> ParserError
  scopeCheck spec
  done "Specification is well-formed!"

mockApp :: MockOptions -> App ()
mockApp opts = do
  bs <- liftIO (try (BS.readFile opts.specFilePath)) <?> ReadSpecFileError
  spec <- pure (runParser specP bs) <?> ParserError
  scopeCheck spec
  (prng, seed) <- liftIO (newPrng opts.seed)
  info
    ( "Starting mock server on http://localhost:"
        <> show (opts.port)
        <> "\n  Use --seed "
        <> show seed
        <> " to reproduce this mock."
    )
  liftIO (runMock opts spec prng)

------------------------------------------------------------------------

testMain :: [String] -> IO ()
testMain args = do
  opts <- parseCliOptions_ args
  mainWith opts True
