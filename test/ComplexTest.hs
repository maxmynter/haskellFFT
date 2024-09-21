module ComplexTest (complexTests) where

import Fft
import Instances ()
import Test.QuickCheck

approxEqual :: Complex -> Complex -> Bool
approxEqual (Cartesian a b) (Cartesian c d) = abs (a - c) < 1e-6 && abs (b - d) < 1e-6
approxEqual (Polar r theta) (Polar r' theta')
  | r < 1e-6 && r' < 1e-6 = True
  | otherwise = abs (r - r') < 1e-6 && abs (theta - theta') < 1e-6
approxEqual a b = approxEqual (asCartesian a) (asCartesian b)

(~==) :: Complex -> Complex -> Property
x ~== y = property $ approxEqual x y

prop_divisionInverse :: Complex -> Complex -> Property
prop_divisionInverse a b =
  b /= Cartesian 0 0 ==>
    let result = (a / b) * b
     in counterexample (show a ++ " / " ++ show b ++ " * " ++ show b ++ " = " ++ show result) $
          result ~== a

prop_expIdentity :: Complex -> Property
prop_expIdentity a =
  let result = exp (log a)
   in counterexample ("exp(log(" ++ show a ++ ")) = " ++ show result) $
        result ~== a

prop_additionCommutative :: Complex -> Complex -> Property
prop_additionCommutative a b = a + b === b + a

prop_multiplicationCommutative :: Complex -> Complex -> Property
prop_multiplicationCommutative a b = a * b === b * a

prop_additionAssociative :: Complex -> Complex -> Complex -> Property
prop_additionAssociative a b c = (a + b) + c === a + (b + c)

prop_multiplicationAssociative :: Complex -> Complex -> Complex -> Property
prop_multiplicationAssociative a b c = (a * b) * c === a * (b * c)

prop_distributive :: Complex -> Complex -> Complex -> Property
prop_distributive a b c = a * (b + c) === a * b + a * c

complexTests :: IO ()
complexTests = do
  putStrLn "Testing Complex operations"
  quickCheck prop_additionCommutative
  quickCheck prop_multiplicationCommutative
  quickCheck prop_additionAssociative
  quickCheck prop_multiplicationAssociative
  quickCheck prop_distributive
  quickCheck prop_divisionInverse
  quickCheck prop_expIdentity
