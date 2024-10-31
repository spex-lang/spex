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

green :: String -> String
green = withSGR [SetColor Foreground Vivid Green]

red :: String -> String
red = withSGR [SetColor Foreground Vivid Red]

boldRed :: String -> String
boldRed = withSGR
  [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]

faint :: String -> String
faint = withSGR [SetConsoleIntensity FaintIntensity]
