{-# LANGUAGE TypeOperators #-}
module TriPointSpec where
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property

import Control.Exception (evaluate)

import qualified Data.Vector as V

import Linear.V3 as L

import TimeIt 

import Bary
import Types
import TriPoint

import ArbLinear
import ArbBary

spec :: Spec
spec = do
  describe "barycentric" $ do
    describe "Native" $ do
      describe "correctness" $ do
        describe "inside" $ do
          it "hits" $ 
            forAll triArb
            ( \ tri -> 
              forAll (baryArb V3) 
              ( \ bary -> 
                property $ (
                  let point = baryToPoint tri  bary
                      res = barycentric tri point
                  in
                    if res == True 
                      then succeeded 
                      else failed
                ) 
              )
            )

        describe "outside" $ do
          let tri = ( V3 (1.0 :: Float) (0.0 :: Float) (0.0 :: Float)
                    , V3 (0.0 :: Float) (1.0 :: Float) (0.0 :: Float)
                    , V3 (0.0 :: Float) (0.0 :: Float) (0.0 :: Float)
                    )
          let point = V3 (1.0 :: Float) (1.0 :: Float) (0.0 :: Float)
          it "misses" $ do
            (barycentric tri point) `shouldBe` False

      describe "performance" $ do
        let dim = 10000000 :: Int
        it (show dim) $ do
          t <- generate triArb
          let pointsArb = vectorOf dim $ fmap (baryToPoint t) (baryArb V3) :: Gen [V3 Float]
          ps <- timeItNamedM "ps" $ generate pointsArb
          vec <- timeItNamed "vec" $ (V.fromListN dim ps :: V.Vector (V3 Float))
          v <- timeItNamed "compute" $ (V.filter (barycentric t) vec)
          pure () 