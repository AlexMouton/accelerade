module ArbBary where

import Test.QuickCheck
import ArbFloat
import Arbys

baryArb :: (Float -> Float -> Float -> v Float ) -> Gen (v Float)
baryArb v = do
  x <- arbFloatUnit
  y <- (*) (1.0 - x) <$> arbFloatUnit
  let z = 1.0 - (x + y)
  return $ v x y z