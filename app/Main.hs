module Main where

import Fft
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

-- Generate a simple polynomial for testing
testPolynomial :: Polynomial
testPolynomial = Polynomial [complexFromInt 1, complexFromInt 2, complexFromInt 3, complexFromInt 4]

-- Expected result after FFT
expectedFftResult :: [Complex]
expectedFftResult =
  [ Cartesian 10 0,
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

-- Function to print FFT results
printFftResults :: Polynomial -> IO ()
printFftResults poly = do
  putStrLn "Input polynomial:"
  print poly
  putStrLn "FFT result:"
  print $ fft poly

-- Function to print comparison of expected and actual results
printComparison :: [Complex] -> [Complex] -> IO ()
printComparison expected actual = do
  putStrLn "Comparison of expected and actual results:"
  mapM_ printComparisonLine $ zip expected actual
  where
    printComparisonLine (e, a) = do
      putStrLn $
        "Expected: "
          ++ show e
          ++ ", Actual: "
          ++ show a
          ++ ", Equal within epsilon: "
          ++ show (complexApproxEqual e a)

-- Main function to run tests and print results
main :: IO ()
main = do
  -- Print FFT results
  printFftResults testPolynomial

  -- Print comparison
  let Polynomial result = fft testPolynomial
  printComparison expectedFftResult result

  -- Run tests
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
