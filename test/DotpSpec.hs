module DotpSpec where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

import Test.Hspec
import System.TimeIt (timeItNamed)

import Dotp

spec :: Spec
spec = do
  let dim = 1000000000
  let xs = fromList (Z :. dim) [0..]   :: Vector Float
  let ys = fromList (Z :. dim) [1,3..] :: Vector Float
  it "runs on cpu" $ do
    timeItNamed "CPU" $ print $ CPU.run $ dotp (use xs) (use ys)
  it "runs on gpu" $ do
    timeItNamed "GPU" $ print $ GPU.run $ dotp (use xs) (use ys)
