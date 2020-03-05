{-# LANGUAGE TypeOperators #-}
module AabbSpec where
import Test.Hspec
import Test.QuickCheck as QC
import Test.QuickCheck.Property


import Data.Vector as V
import Linear.V3

import TimeIt 
import Aabb
import ArbLinear

specAabb dim = do
  it "prints" $ do
    let pointsArb = vectorOf dim $ (v3Arb V3 arbitrary) :: Gen [V3 Float]
    ps <- timeItNamedM "ps" $ QC.generate pointsArb
    vec <- timeItNamed "vec" $ fromListN dim ps :: IO (Vector (V3 Float))
    v <- timeItNamed "compute" $ V.minimum vec
    print v

spec :: Spec
spec = do
  describe "Aabb" $ do
    let dim = 1000000 :: Int
    specAabb dim