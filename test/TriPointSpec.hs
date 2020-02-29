{-# LANGUAGE TypeOperators #-}
module TriPointSpec where
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property

import System.TimeIt (timeItNamed)

import Debug.Trace

import qualified Linear.V3 as L
import qualified Data.Array.Accelerate.Linear.V3 as AL

import Control.Exception (evaluate)
import Control.Monad (void)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate ( (:.), use )
import qualified Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.LLVM.PTX as GPU
import Unsafe.Coerce (unsafeCoerce)

import TriPoint
import Types
import Arbys

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
      let dim = 10000 :: Int
      it "prints" $ do
        t <- generate triArb
        let pointsArb = vectorOf dim $ fmap (baryToPoint t) (baryArb AL.V3) :: Gen [AL.V3 Float]
        ps <- generate pointsArb
        let baryNative = filter (barycentric t)
        timeItNamed "cpu" $ print $ baryNative ps

  describe "Acc" $ do
    describe "barycentri" $ do
      let dim = 10000 :: Int
      it "prints" $ do
        t <- generate triArb
        let pointsArb = vectorOf dim $ fmap (baryToPoint t) (baryArb AL.V3) :: Gen [AL.V3 Float]
        ps <- generate pointsArb
        let vec = A.fromList (A.Z A.:. dim) ps :: A.Vector (AL.V3 Float)
        let baryCpu = CPU.runN $ A.filter (barycentricExp $ A.lift t) :: A.Array (A.DIM0 :. Int) (AL.V3 Float) -> (A.Vector (AL.V3 Float), A.Array A.DIM0 Int)
        let baryGpu = GPU.runN $ A.filter (barycentricExp $ A.lift t) :: A.Array (A.DIM0 :. Int) (AL.V3 Float) -> (A.Vector (AL.V3 Float), A.Array A.DIM0 Int)
        evaluate baryCpu
        evaluate baryGpu
        timeItNamed "cpu" $ print $ baryCpu vec
        timeItNamed "gpu" $ print $ baryGpu vec 
        -- timeItNamed "gpu" $ print $ GPU.run $ A.filter (\_ -> (A.constant True)) (use vec)

      -- describe "Crash" $ do
      --   describe "list bootstratp" $ do
      --     let dim = 1000 :: Int
      --     it "print" $ do
      --       let vec = A.fromList (A.Z A.:. dim) [1..dim] :: A.Vector Int
      --       timeItNamed "gpu" $ print $ GPU.run $ A.filter (\_ -> A.lift True) (A.use vec)

      --   -- describe "accelerated v3 float" $ do
      --   --   it "print" $ do
      --   --     let vec = A.fromList (A.Z A.:. 2) [AL.V3 1 1 1, AL.V3 0 0 0] ::  A.Array (A.Z A.:. Int) (AL.V3 Float)
      --   --     timeItNamed "cpu" $ print $ CPU.run $ A.filter (\_ -> A.lift True) (use vec)
      --   --     timeItNamed "gpu" $ print $ GPU.run $ A.filter (\_ -> A.lift True) (use vec)

      --   describe "accelerated t3 float" $ do
      --     it "print" $ do
      --       let vec = A.fromList (A.Z A.:. 2) [(1, 1, 1), (0, 0, 0)] ::  A.Array (A.Z A.:. Int) (Float, Float, Float)
      --       timeItNamed "cpu" $ print $ CPU.run $ A.filter (\_ -> A.lift True) (use vec)
      --       -- timeItNamed "gpu" $ print $ GPU.run $ A.filter (\_ -> A.lift True) (use vec)
