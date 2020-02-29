{-# LANGUAGE ScopedTypeVariables #-}
module TriPoint where

import qualified Linear.V3 as L

import qualified Data.Array.Accelerate.Linear.V3 as AL
import qualified Data.Array.Accelerate.Linear.Metric as ALM

import Linear.Metric as LM

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Exp(..), Acc, Vector)

import Debug.Trace

import Types

-- traceThru :: Show  a => String -> a -> a
-- traceThru s a = traceShow (s ++ " " ++ show a) a
-- traceThru _ a = a

-- Z :. 3 :. 3
-- Z :. 1 : 3

barycentric :: Triangle (L.V3 Float) -> L.V3 Float -> Bool
barycentric (a, b, c) p =
  let
    -- map
    v0 = c - a
    v1 = b - a
    v2 = p - a

    -- Compute dot products
    dot00 = LM.dot v0 v0
    dot01 = LM.dot v0 v1
    dot02 = LM.dot v0 v2
    dot11 = LM.dot v1 v1
    dot12 = LM.dot v1 v2

    -- Compute barycentric coordinates
    denom = (dot00 * dot11 - dot01 * dot01)
    invDenom = 1.0 / denom
    u = (dot11 * dot02 - dot01 * dot12) * invDenom
    v = (dot00 * dot12 - dot01 * dot02) * invDenom
  in
    -- Check if point is in triangle
    (v >= 0.0) && (u >= 0.0) && (u + v < 1.0)

barycentricExp :: A.Exp (Triangle (AL.V3 Float)) -> A.Exp (AL.V3  Float) -> A.Exp Bool
barycentricExp et p = 
   let 
    (a, b, c) = A.unlift et :: (Exp (AL.V3 Float), Exp (AL.V3 Float), Exp (AL.V3 Float))
    v0 = c - a
    v1 = b - a
    v2 = p - a

    -- Compute dot products
    dot00 = ALM.dot v0 v0
    dot01 = ALM.dot v0 v1
    dot02 = ALM.dot v0 v2
    dot11 = ALM.dot v1 v1
    dot12 = ALM.dot v1 v2

    -- Compute barycentric coordinates
    denom = (dot00 * dot11 - dot01 * dot01)
    invDenom = 1.0 / denom
    u = (dot11 * dot02 - dot01 * dot12) * invDenom
    v = (dot00 * dot12 - dot01 * dot02) * invDenom
  in 
    A.lift $ (v A.>= 0.0) A.&& (u A.>= 0.0) A.&& (u + v A.< 1.0)

-- barycentricExp :: A.Exp (Triangle (AL.V3 Float)) -> A.Exp (AL.V3 Float) -> A.Exp Bool
-- barycentricExp _ _ = A.constant True

-- barycentricExp :: A.Exp (Triangle (Point Float)) -> A.Exp (Point Float) -> A.Exp Bool
-- barycentricExp et p = 
--   -- let 
--     -- t = A.unlift et :: (Exp (AL.V3 Float), Exp (AL.V3 Float), Exp (AL.V3 Float))
--   -- in 
--     (A.lift2 barycentric) et p

-- barycentricSingleTriAcc :: Acc (Triangle Float) -> Acc (Vector (Point Float)) -> Acc (Vector (Point Float))
-- barycentricSingleTriAcc t = A.filter (const True) --(barycentric t)

-- barycentricSinglePointAcc :: Acc (Point Float) -> Acc (Vector(Triangle Float)) -> Acc (Vector (Triangle Float))
-- barycentricSinglePointAcc p = A.filter ((flip barycentricExp) p)

-- barycentricCrossAcc :: Acc (Vector(Triangle Float)) -> Acc (Vector (Point Float)) -> Acc (Vector (Point Float, Triangle Float))
-- barycentricSinglePointAcc t ps = 
