module Bary where

import qualified Linear.V3 as L

import Types

baryToPoint :: Triangle (L.V3 Float) -> L.V3 Float -> L.V3 Float
baryToPoint (p0, p1, p2) (L.V3 b0 b1 b2) = 
  p0 * pure b0 
  + p1 * pure b1 
  + p2 * pure b2

