module Spex.CommandLine.Ansi where

import System.Console.ANSI
import System.IO

------------------------------------------------------------------------

hasAnsiSupport :: IO Bool
hasAnsiSupport = hNowSupportsANSI stdout

withSGR :: [SGR] -> String -> String
withSGR sgr s = setSGRCode sgr ++ s ++ setSGRCode []

cyan :: String -> String
cyan = withSGR [SetColor Foreground Vivid Cyan]

boldRed :: String -> String
boldRed = withSGR
  [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
