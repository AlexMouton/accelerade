module TriPoint where

import Data.Array.Accelerate.Linear.V3 (V3(..))
import qualified Linear.V3 as L
-- import Linear.Vector ((^-^))

import Linear.Metric (dot)

import Debug.Trace

-- traceThru :: Show  a => String -> a -> a
traceThru s a = traceShow (s ++ " " ++ show a) a
-- traceThru _ a = a

type Triangle a = (L.V3 a, L.V3 a, L.V3 a)
type Point a = L.V3 a

barycentric :: Triangle Float -> Point Float -> Bool
barycentric (a, b, c) p =
  let
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
    u = if denom == 0.0 then 0.0 else (dot11 * dot02 - dot01 * dot12) * invDenom
    v = if denom == 0.0 then 0.0 else (dot00 * dot12 - dot01 * dot02) * invDenom
  in
    -- Check if point is in triangle
    (v >= 0.0) && (u >= 0.0) && (u + v < 1.0)
