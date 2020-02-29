{-# LANGUAGE TypeOperators #-}
module TriPointSpec where
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property

import System.TimeIt (timeItNamed, timeItT)

import Debug.Trace

import qualified Linear.V3 as L
import qualified Data.Array.Accelerate.Linear.V3 as AL

import Control.Exception (evaluate)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate ( (:.) )

import qualified Data.Array.Accelerate.LLVM.Native as CPU
import qualified Data.Array.Accelerate.LLVM.PTX as GPU

import Unsafe.Coerce (unsafeCoerce)

import TriPoint
import Types
import Arbys
import SupportAcc 

baryArb :: (Float -> Float -> Float -> v Float ) -> Gen (v Float)
baryArb v = do
  x <- arbFloatUnit
  y <- (*) (1.0 - x) <$> arbFloatUnit
  let z = 1.0 - (x + y)
  return $ v x y z

baryToPoint :: Triangle (L.V3 Float) -> L.V3 Float -> L.V3 Float
baryToPoint (p0, p1, p2) (L.V3 b0 b1 b2) = 
  p0 * pure b0 
  + p1 * pure b1 
  + p2 * pure b2

specBariAcc dim runN = do
  it "prints" $ do
    t <- generate triArb
    let pointsArb = vectorOf dim $ fmap (baryToPoint t) (baryArb AL.V3) :: Gen [AL.V3 Float]
    ps <- generate pointsArb
    let vec = A.fromList (A.Z A.:. dim) ps :: A.Vector (AL.V3 Float)
    let baryN = runN $ A.filter (barycentricExp $ A.lift t) :: A.Array (A.DIM0 :. Int) (AL.V3 Float) -> (A.Vector (AL.V3 Float), A.Array A.DIM0 Int)
    evaluate baryN
    (t, v) <- timeItT $ pure $ baryN vec
    print t

spec :: Spec
spec = do
  describe "tripoint" $ do
    describe "barycentric" $ do
      describe "Native" $ do
        describe "inside" $ do
          it "hits" $ property $ do
            tri <- triArb
            bary <- baryArb L.V3
            let point = baryToPoint tri  bary
            let res = barycentric tri point
            return $ if res Prelude.== True 
              then succeeded 
              else failed

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
        let pointsArb = vectorOf dim $ fmap (baryToPoint t) (baryArb AL.V3) :: Gen [AL.V3 Float]
        ps <- generate pointsArb
        let baryNative = filter (barycentric t)
        (t, v) <- timeItT $ pure $ baryNative ps
        print t

  describe "Acc" $ do
      let dim = 100000000 :: Int
      specPair "baricentric" $ specBariAcc dim