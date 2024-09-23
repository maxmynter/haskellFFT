module Main where

import Fft
import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

-- Generate a simple polynomial for testing
testPolynomial :: Polynomial
testPolynomial = Polynomial [complexFromInt 1, complexFromInt 2, complexFromInt 3, complexFromInt 4]

-- Expected result after FFT
expectedFftResult :: [Complex]
expectedFftResult = [
    Cartesian 10 0,
    Cartesian (-2) 2,
    Cartesian (-2) 0,
    Cartesian (-2) (-2)
  ]

-- Epsilon for floating-point comparisons
epsilon :: Double
epsilon = 1e-6

-- Compare two complex numbers with epsilon
complexApproxEqual :: Complex -> Complex -> Bool
complexApproxEqual a b =
  abs (real a - real b) < epsilon && abs (im a - im b) < epsilon

-- Test function
testFft :: Test
testFft = TestCase $ do
  let Polynomial result = fft testPolynomial
  assertEqual "FFT result length" (length expectedFftResult) (length result)
  assertBool "FFT result values" $ and $ zipWith complexApproxEqual expectedFftResult result

-- List of all tests
tests :: Test
tests = TestList [TestLabel "FFT Test" testFft]

-- Main function to run tests
main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
