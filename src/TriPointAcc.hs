{-# LANGUAGE ScopedTypeVariables #-}
module TriPointAcc where

import Prelude()

import Data.Array.Accelerate.Linear.V3 as L
import Data.Array.Accelerate.Linear.Metric as LM

import Data.Array.Accelerate as A
import Data.Array.Accelerate (Exp(..), Acc, Vector)

import Debug.Trace

import Types

barycentricExp :: Exp (Triangle (V3 Float)) -> Exp (V3  Float) -> Exp Bool
barycentricExp et p = 
   let 
    (a, b, c) = A.unlift et :: (Exp (V3 Float), Exp (V3 Float), Exp (V3 Float))
    v0 = c - a
    v1 = b - a
    v2 = p - a

    -- Compute dot products
    dot00 = dot v0 v0
    dot01 = dot v0 v1
    dot02 = dot v0 v2
    dot11 = dot v1 v1
    dot12 = dot v1 v2

    -- Compute barycentric coordinates
    denom = (dot00 * dot11 - dot01 * dot01)
    invDenom = 1.0 / denom
    u = (dot11 * dot02 - dot01 * dot12) * invDenom
    v = (dot00 * dot12 - dot01 * dot02) * invDenom
  in 
    A.lift $ (v A.>= 0.0) A.&& (u A.>= 0.0) A.&& (u + v A.< 1.0)