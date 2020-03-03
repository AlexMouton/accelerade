{-# LANGUAGE TypeOperators #-}
module TriPointAccSpec where
import Test.Hspec
import Test.QuickCheck as QC
import Test.QuickCheck.Property

import Control.Exception (evaluate)

import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate as A

import System.TimeIt (timeItNamed, timeItT)

import Bary
import Types
import TriPointAcc

import SupportAcc

import ArbLinear
import ArbBary

specBariAcc dim runN = do
    t <- QC.generate triArb
    let pointsArb = vectorOf dim $ fmap (baryToPoint t) (baryArb V3) :: Gen [V3 Float]
    ps <- QC.generate pointsArb
    vec <- evaluate $ fromList (Z :. dim) ps :: IO (Vector (V3 Float))
    baryN <- evaluate $ runN $ A.filter (barycentricExp $ lift t) :: IO (Array (DIM0 :. Int) (V3 Float) -> (Vector (V3 Float), Array A.DIM0 Int))
    evaluate baryN
    (t, v) <- timeItT $ evaluate $ baryN vec
    print $ "t: " <> (show t) <>  " sec"

spec :: Spec
spec = do
  describe "TriPointAcc" $ do
    let dim = 1000000 :: Int
    specPair "baricentric" $ specBariAcc dim