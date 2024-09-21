module Main where

import ComplexTest (complexTests)
import PolynomialTest (polynomialTests)
import System.Exit (exitFailure, exitSuccess)
import Prelude (IO, putStrLn, (&&))

main :: IO ()
main = do
  putStrLn "Running Tests: "
  putStrLn ""
  complexResults <- complexTests
  polynomialResults <- polynomialTests

  let allPass = complexResults && polynomialResults
  if allPass
    then do
      putStrLn "All tests passed"
      exitSuccess
    else do
      putStrLn "Some tests failed"
      exitFailure
