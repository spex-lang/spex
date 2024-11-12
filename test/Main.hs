module Main (main) where

import Control.Concurrent
import Control.Exception
import Data.List (intercalate)
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.Golden (goldenVsFileDiff)

import Petstore qualified
import Spex.LibMain

------------------------------------------------------------------------

tests :: FilePath -> TestTree
tests tmpDir =
  testGroup
    "Golden"
    [ withPetstore
        0
        ( test
            [ "verify"
            , "example/petstore-basic.spex"
            , "--seed"
            , "2503963955766725184"
            ]
        )
    ]
  where
    test :: [String] -> TestTree
    test args =
      let testName = intercalate " " args
          logName = replace '/' '-' (intercalate "_" args)
          logFile = tmpDir </> logName <.> "log"
      in  goldenVsFileDiff
            testName
            (\ref new -> ["diff", "-u", ref, new])
            ("test" </> "golden" </> logName <.> "golden")
            logFile
            (testMain ("--log-file" : logFile : args))

    withPetstore :: Int -> TestTree -> TestTree
    withPetstore i tt =
      withResource
        (forkIO (Petstore.libMain (8080 + i)))
        killThread
        (const tt)

    replace :: Char -> Char -> String -> String
    replace needle replacement = go
      where
        go [] = []
        go (c : cs)
          | c == needle = replacement : go cs
          | otherwise = c : go cs

main :: IO ()
main = do
  tmp <- getTemporaryDirectory
  bracket
    (createTempDirectory tmp "spex-golden-tests")
    removePathForcibly
    $ \tmpDir -> defaultMain (tests tmpDir)
