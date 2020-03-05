module DotpSpec where

import Control.Exception (evaluate)

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

import Test.Hspec

import TimeIt 
import SupportAcc 
import Dotp

specDotpN dim runN = do
  xs <- timeItNamed "xs" $ fromList (Z :. dim) [0..]
  ys <- timeItNamed "ys" $ fromList (Z :. dim) [1,3..]
  dotN <- timeItNamed "runN" $ (runN dotp :: Array DIM1 Float -> Array DIM1 Float -> Array DIM0 Float)
  timeItNamed "evaluate" $ evaluate dotN
  v <- timeItNamed "compute" $ dotN xs ys
  pure ()

spec :: Spec
spec = do
  let dim = 100000000
  specPair "dotp" $ specDotpN dim
