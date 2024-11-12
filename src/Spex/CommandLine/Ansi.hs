module Spex.CommandLine.Ansi where

import Data.Text (Text)
import Data.Text qualified as Text
import System.Console.ANSI
import System.IO

------------------------------------------------------------------------

hasAnsiSupport :: IO Bool
hasAnsiSupport = hNowSupportsANSI stdout

withSGR :: [SGR] -> Text -> Text
withSGR sgr s = Text.pack (setSGRCode sgr) <> s <> Text.pack (setSGRCode [])

cyan :: Text -> Text
cyan = withSGR [SetColor Foreground Vivid Cyan]

green :: Text -> Text
green = withSGR [SetColor Foreground Vivid Green]

red :: Text -> Text
red = withSGR [SetColor Foreground Vivid Red]

boldRed :: Text -> Text
boldRed =
  withSGR
    [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]

faint :: Text -> Text
faint = withSGR [SetConsoleIntensity FaintIntensity]
