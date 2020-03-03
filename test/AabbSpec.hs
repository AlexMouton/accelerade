{-# LANGUAGE TypeOperators #-}
module AabbSpec where
import Test.Hspec
import Test.QuickCheck as QC
import Test.QuickCheck.Property

import Control.Exception (evaluate)

import Data.Vector as V

import System.TimeIt (timeItNamed, timeItT)

import Linear.V3

import Aabb

import ArbLinear

specAabb dim = do
  it "prints" $ do
    let pointsArb = vectorOf dim $ (v3Arb V3 arbitrary) :: Gen [V3 Float]
    ps <- QC.generate pointsArb
    let vec = fromListN dim ps :: Vector (V3 Float)
    (t, v) <- timeItT $ evaluate $ V.minimum vec
    putStrLn $ "t: " <> (show t) <>  " sec"
    print v

spec :: Spec
spec = do
  describe "Aabb" $ do
    let dim = 1000000 :: Int
    specAabb dim