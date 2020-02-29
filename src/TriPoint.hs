{-# LANGUAGE ScopedTypeVariables #-}
module TriPoint where

import Linear.V3 as L
import Linear.Metric as LM

import Types

import Debug.Trace

barycentric :: Triangle (V3 Float) -> V3 Float -> Bool
barycentric (a, b, c) p =
  let
    -- map
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
    -- Check if point is in triangle
    (v >= 0.0) && (u >= 0.0) && (u + v < 1.0)