module AabbAcc where

import Prelude()

import Control.Arrow ((&&&))

import Data.Array.Accelerate.Linear.V3

import Data.Array.Accelerate as A

type Bounds = (V3 Float, V3 Float) 

aabb :: Acc (Vector(V3 Float)) -> Acc (Array DIM0 (V3 Float))
aabb = A.minimum