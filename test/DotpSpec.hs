module DotpSpec where

import Control.Exception (evaluate)

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

import Test.Hspec
import System.TimeIt (timeItNamed)

import Dotp

spec :: Spec
spec = do
  let dim = 1000000000
  let xs = fromList (Z :. dim) [0..]
  let ys = fromList (Z :. dim) [1,3..]
  let dotCpu = CPU.runN dotp
  let dotGpu = GPU.runN dotp
  it "runs on cpu" $ do
    evaluate dotCpu
    timeItNamed "CPU" $ print $ dotCpu xs ys
  it "runs on gpu" $ do
    evaluate dotGpu
    timeItNamed "GPU" $ print $ dotGpu xs ys
