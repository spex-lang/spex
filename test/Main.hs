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
    [ testGroup
        "Verify"
        ( zipWith
            withPetstore
            [ test
                [ "verify"
                , "example/petstore-basic.spex"
                , "--seed"
                , "2503963955766725184"
                ]
            , test
                [ "verify"
                , "example/petstore-modal-faults.spex"
                , "--tests"
                , "2000"
                , "--seed"
                , "3967796076964233976"
                ]
            ]
            [0 ..] -- Used to compute the ports used by the tests.
        )
    , testGroup
        "Format"
        [ testOutput ["format", "example/petstore-bad-formatting.spex"]
        ]
    ]
  where
    test :: [String] -> Int -> TestTree
    test args i =
      let testName = intercalate " " args
          logName = replace '/' '-' (intercalate "_" args)
          logFile = tmpDir </> logName <.> "log"
      in  goldenVsFileDiff
            testName
            (\ref new -> ["diff", "-u", ref, new])
            ("test" </> "golden" </> logName <.> "golden")
            logFile
            ( testMain
                ( "--non-interactive"
                    : "--log-file"
                    : logFile
                    : args
                    ++ ["--port", show (8080 + i)]
                )
            )

    testOutput :: [String] -> TestTree
    testOutput args =
      let testName = intercalate " " args
          outName = replace '/' '-' (intercalate "_" args)
          outFile = tmpDir </> outName <.> "txt"
      in  goldenVsFileDiff
            testName
            (\ref new -> ["diff", "-u", ref, new])
            ("test" </> "golden" </> outName <.> "golden")
            outFile
            (testMain ("--non-interactive" : args ++ ["--output", outFile]))

    withPetstore :: (Int -> TestTree) -> Int -> TestTree
    withPetstore tt i =
      withResource
        (forkIO (Petstore.libMain (8080 + i)))
        killThread
        (const (tt i))

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
