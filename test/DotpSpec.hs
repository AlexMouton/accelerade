module DotpSpec where

import Control.Exception (evaluate)

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

import Test.Hspec
import System.TimeIt (timeItNamed)

import Dotp

specDotpN dim runN = do
  let xs = fromList (Z :. dim) [0..]
  let ys = fromList (Z :. dim) [1,3..]
  let dotN = runN dotp :: Array DIM1 Float -> Array DIM1 Float -> Array DIM0 Float
  it "runs" $ do
    evaluate dotN
    timeItNamed "dotN" $ print $ dotN xs ys

spec :: Spec
spec = do
  let dim = 10000000
  describe "on cpu" $ do
    specDotpN dim CPU.runN
  describe "on gpu" $ do
    specDotpN dim GPU.runN
