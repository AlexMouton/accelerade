{-# LANGUAGE TypeOperators #-}
module TriPointAccSpec where
import Test.Hspec
import Test.QuickCheck as QC
import Test.QuickCheck.Property

import Control.Exception (evaluate)
import Control.Monad (join)

import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate as A

import TimeIt 

import Bary
import Types
import TriPointAcc

import SupportAcc

import ArbLinear
import ArbBary

specBariAcc dim runN = do
    t <- timeItNamedM "t" $ QC.generate triArb
    let pointsArb = vectorOf dim $ fmap (baryToPoint t) (baryArb V3) :: Gen [V3 Float]
    ps <- timeItNamedM "ps" $ QC.generate pointsArb
    vec <- timeItNamed "vec" $ fromList (Z :. dim) ps :: IO (Vector (V3 Float))
    baryN <- timeItNamed "runN" $ runN $ A.filter (barycentricExp $ lift t) :: IO (Array (DIM0 :. Int) (V3 Float) -> (Vector (V3 Float), Array A.DIM0 Int))
    timeItNamedM "evaluate" $ evaluate baryN
    v <- timeItNamed "compute" $ baryN vec
    pure ()

spec :: Spec
spec = do
  describe "TriPointAcc" $ do
    let dim = 1000000 :: Int
    specPair "baricentric" $ specBariAcc dim