{-# LANGUAGE TypeOperators #-}

module AabbAccSpec where
import Test.Hspec
import Test.QuickCheck as QC
import Test.QuickCheck.Property

import Control.Exception (evaluate)
import Control.Monad (join)

import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate as A

import TimeIt 

-- import Bary
-- import Types
import AabbAcc 

import SupportAcc

import ArbLinear
-- import ArbBary

specAabbAcc dim runN = do
    let pointsArb = vectorOf dim $ (v3Arb V3 arbitrary) :: Gen [V3 Float]
    ps <- timeItNamedM "ps" $ QC.generate pointsArb
    vec <- timeItNamed "vec" $ fromList (Z :. dim) ps :: IO (Vector (V3 Float))
    aabbN <- timeItNamed "runN" $ runN $ aabb :: IO (Vector(V3 Float) -> Array DIM0 (V3 Float))
    -- evaluate aabbN
    -- let t = aabbN vec
    v <- timeItNamed "compute" $ aabbN vec
    print v

spec :: Spec
spec = do
  describe "AabbAcc" $ do
    let dim = 1000000 :: Int
    specPair "aabb" $ specAabbAcc dim