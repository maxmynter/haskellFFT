module Main where

import Fft
import System.Exit (exitFailure)
import Test.QuickCheck

instance Arbitrary Complex where
  arbitrary = Complex <$> arbitrary <*> arbitrary

instance Arbitrary Polynomial where
  arbitrary = Polynomial <$> arbitrary

prop_splitRecombine :: Polynomial -> Property
prop_splitRecombine p@(Polynomial cs) =
  let (Polynomial evenPoly, Polynomial oddPoly) = splitPolynomial p
      recombined = interleave evenPoly oddPoly
   in counterexample
        ( "Original: "
            ++ show cs
            ++ "\nEven part: "
            ++ show evenPoly
            ++ "\nOdd part: "
            ++ show oddPoly
            ++ "\nRecombined: "
            ++ show recombined
        )
        (cs == recombined)

interleave :: [a] -> [a] -> [a]
interleave (x : xs) (y : ys) = x : y : interleave xs ys
interleave [] ys = ys
interleave xs [] = xs

main :: IO ()
main = do
  putStrLn "Testing Polynomial"
  result <- quickCheckResult prop_splitRecombine
  case result of
    Success {} -> putStrLn "All tests passed!"
    _ -> do
      putStrLn "Test failed!"
      exitFailure
