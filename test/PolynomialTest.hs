module PolynomialTest (polynomialTests) where

import Fft
import Instances ()
import Test.QuickCheck
import TestUtils (runNamedTest)

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

polynomialTests :: IO Bool
polynomialTests = do
  putStrLn "Testing Polynomial"
  putStrLn "---------------"
  results <-
    sequence
      [ runNamedTest "Exponential Identity" prop_splitRecombine
      ]
  return (and results)
