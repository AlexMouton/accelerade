module Main where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU
import System.TimeIt (timeItNamed)

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

main :: IO ()
main = do
  let dim = 1000000000
  let xs = fromList (Z :. dim) [0..]   :: Vector Float
  let ys = fromList (Z :. dim) [1,3..] :: Vector Float
  timeItNamed "CPU" $ print $ CPU.run $ dotp (use xs) (use ys)
  timeItNamed "GPU" $ print $ GPU.run $ dotp (use xs) (use ys)
  print 5
