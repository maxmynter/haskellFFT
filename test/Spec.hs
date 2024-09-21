module Main where

import ComplexTest (complexTests)
import PolynomialTest (polynomialTests)
import System.Exit (exitFailure)
import Test.QuickCheck

main :: IO ()
main = do
  complexTests
  polynomialTests
  putStrLn "All tests passed!"
