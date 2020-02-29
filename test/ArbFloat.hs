module ArbFloat where
import Test.QuickCheck
import Test.QuickCheck.Property

import Data.Fixed (mod')

arbFloatPos :: Gen Float
arbFloatPos = fmap abs arbitrary

arbFloatPosEps :: Gen Float
arbFloatPosEps = fmap ((+) (0.01 :: Float)) arbFloatPos 

arbFloatUnit :: Gen Float
arbFloatUnit = mod1 <$> arbitrary
  where
    mod1 :: Float  -> Float
    mod1 x = (abs x) `mod'` 1.0