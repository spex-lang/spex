module Spex.LibMain where

import Control.Exception
import qualified Data.ByteString as BS
import Data.Maybe
import System.Console.ANSI
import System.Exit
import System.IO

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
  runApp defaultAppEnv (app args) >>= \case
    Left err -> do
      putStrLn ""
      stdoutSupportsANSI <- hNowSupportsANSI stdout
      if stdoutSupportsANSI
      then do
        setSGR [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
        putStr "Error: "
        setSGR [Reset]
      else
        putStr "Error: "
      putStrLn (displayAppError err)
      exitFailure
    Right () -> exitSuccess

app :: CmdLineArgs -> App ()
app args = do
  info $ "Checking:   " <> args.specFilePath --  <> BS.unpack spec.component.id
  let config = Config (fromMaybe 100 args.numTests) args.seed
      deploy = Deployment (HostPort args.host args.port)
                 (HealthCheckPath args.health) (ResetPath args.reset)
  info $ "Deployment: " <> args.host <> ":" <> show args.port
  bs <- liftIO (try (BS.readFile args.specFilePath))
          <?> ReadSpecFileError args.specFilePath
  spec <- pure (runParser specP bs) <?> ParserError
  verify config spec deploy
