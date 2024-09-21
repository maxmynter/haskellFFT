module Instances where

import Fft
import Test.QuickCheck

instance Arbitrary Complex where
  arbitrary =
    oneof
      [ Cartesian <$> arbitrary <*> arbitrary,
        Polar . abs <$> arbitrary <*> arbitrary
      ]

instance Arbitrary Polynomial where
  arbitrary = Polynomial <$> arbitrary
