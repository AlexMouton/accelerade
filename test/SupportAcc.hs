module SupportAcc where

import Test.Hspec

import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

specPair name s = do 
  it "on cpu" $ do
    s CPU.runN
  it "on gpu" $ do
    s GPU.runN