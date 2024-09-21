module ComplexTest (complexTests) where

import Fft
import Instances ()
import Test.QuickCheck
import TestUtils (runNamedTest)

approxEqual :: Complex -> Complex -> Bool
approxEqual (Cartesian r1 i1) (Cartesian r2 i2) = let bound = 10e-6 in abs (r1 - r2) < bound && abs (i1 - i2) < bound
approxEqual z1 z2 = approxEqual (asCartesian z1) (asCartesian z2)

infix 4 ~==

(~==) :: Complex -> Complex -> Property
x ~== y = counterexample (show x ++ " /≈ " ++ show y) $ property (approxEqual x y)

prop_divisionInverse :: Complex -> Complex -> Property
prop_divisionInverse a b =
  mag a > 1e-6 && mag b > 1e-6 ==> a / b * b ~== a

prop_expIdentity :: Complex -> Property
prop_expIdentity a =
  mag a > 1e-4 ==> exp (log a) ~== a

prop_additionCommutative :: Complex -> Complex -> Property
prop_additionCommutative a b = a + b ~== b + a

prop_multiplicationCommutative :: Complex -> Complex -> Property
prop_multiplicationCommutative a b = a * b ~== b * a

prop_additionAssociative :: Complex -> Complex -> Complex -> Property
prop_additionAssociative a b c = (a + b) + c ~== a + (b + c)

prop_multiplicationAssociative :: Complex -> Complex -> Complex -> Property
prop_multiplicationAssociative a b c = (a * b) * c ~== a * (b * c)

prop_distributive :: Complex -> Complex -> Complex -> Property
prop_distributive a b c = a * (b + c) ~== a * b + a * c

complexTests :: IO Bool
complexTests = do
  putStrLn "Running Complex Number Tests:"
  putStrLn "---------------"
  results <-
    sequence
      [ runNamedTest "Addition Commutative" prop_additionCommutative,
        runNamedTest "Multiplication Commutative" prop_multiplicationCommutative,
        runNamedTest "Addition Associative" prop_additionAssociative,
        runNamedTest "Multiplication Associative" prop_multiplicationAssociative,
        runNamedTest "Distributive Property" prop_distributive,
        runNamedTest "Division Inverse" prop_divisionInverse,
        runNamedTest "Exponential Identity" prop_expIdentity
      ]
  return (and results)
