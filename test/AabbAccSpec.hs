{-# LANGUAGE TypeOperators #-}

module AabbAccSpec where
import Test.Hspec
import Test.QuickCheck as QC
import Test.QuickCheck.Property

import Control.Exception (evaluate)

import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate as A

import System.TimeIt (timeItNamed, timeItT)

-- import Bary
-- import Types
import AabbAcc 

import SupportAcc

import ArbLinear
-- import ArbBary

specAabbAcc dim runN = do
    let pointsArb = vectorOf dim $ (v3Arb V3 arbitrary) :: Gen [V3 Float]
    ps <- QC.generate pointsArb
    let vec = fromList (Z :. dim) ps :: Vector (V3 Float)
    let aabbN = runN $ aabb :: Vector(V3 Float) -> Array DIM0 (V3 Float)
    evaluate aabbN
    -- let t = aabbN vec
    (t, v) <- timeItT $ evaluate $ aabbN vec
    print $ "t: " <> (show t) <>  " sec"
    -- print t
    print v

spec :: Spec
spec = do
  describe "AabbAcc" $ do
    let dim = 1000000 :: Int
    specPair "aabb" $ specAabbAcc dim