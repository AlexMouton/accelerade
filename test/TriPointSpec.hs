{-# LANGUAGE TypeOperators #-}
module TriPointSpec where
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property

import Control.Exception (evaluate)

import qualified Linear.V3 as L

import System.TimeIt (timeItNamed, timeItT)

import Bary
import Types
import TriPoint

import SupportAcc

import ArbLinear
import ArbBary

spec :: Spec
spec = do
  describe "tripoint" $ do
    describe "barycentric" $ do
      describe "Native" $ do
        describe "inside" $ do
          it "hits" $ 
            forAll triArb
            ( \ tri -> 
              forAll (baryArb L.V3) 
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
          let tri = ( L.V3 (1.0 :: Float) (0.0 :: Float) (0.0 :: Float)
                    , L.V3 (0.0 :: Float) (1.0 :: Float) (0.0 :: Float)
                    , L.V3 (0.0 :: Float) (0.0 :: Float) (0.0 :: Float)
                    )
          let point = L.V3 (1.0 :: Float) (1.0 :: Float) (0.0 :: Float)
          it "misses" $ do
            (barycentric tri point) `shouldBe` False

    describe "barycentric" $ do
      let dim = 100000000 :: Int
      it "prints" $ do
        t <- generate triArb
        let pointsArb = vectorOf dim $ fmap (baryToPoint t) (baryArb L.V3) :: Gen [L.V3 Float]
        ps <- generate pointsArb
        let baryNative = filter (barycentric t)
        (t, v) <- timeItT $ pure $ baryNative ps
        print t