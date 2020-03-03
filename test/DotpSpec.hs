module DotpSpec where

import Control.Exception (evaluate)

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

import Test.Hspec
import System.TimeIt (timeItT)

import SupportAcc 
import Dotp

specDotpN dim runN = do
  xs <- evaluate $ fromList (Z :. dim) [0..]
  ys <- evaluate $ fromList (Z :. dim) [1,3..]
  dotN <- evaluate $ runN dotp :: IO (Array DIM1 Float -> Array DIM1 Float -> Array DIM0 Float)
  evaluate dotN
  (t, v) <- timeItT $ evaluate $ dotN xs ys
  print $ "t: " <> (show t) <>  " sec"

spec :: Spec
spec = do
  let dim = 100000000
  specPair "dotp" $ specDotpN dim
