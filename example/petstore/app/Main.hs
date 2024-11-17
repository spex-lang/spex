module Main where

import Petstore

import System.IO

------------------------------------------------------------------------

main :: IO ()
main = do
  let port = 8080
  hPutStrLn stderr $ "Running petstore on localhost:" <> show port
  libMain port
