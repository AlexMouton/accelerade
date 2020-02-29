module ArbLinear where
import Test.QuickCheck
import Test.QuickCheck.Property

import qualified Linear.V3 as L
import TriPoint
import Types

import ArbFloat

type D3 v = Float -> Float -> Float -> v Float

v3Arb :: D3 v -> Gen Float -> Gen (v Float)
v3Arb v f = v <$> f <*> f <*> f 

normArb :: D3 L.V3 -> Gen (L.V3 Float)
normArb v = do 
  phi <- (*) pi <$> arbFloatUnit
  theta <- (*) (2.0 * pi ) <$> arbFloatUnit
  let x = sin phi * cos theta
  let y = sin phi * sin theta 
  let z = cos phi
  return $ v x y z

basisArb :: Gen (L.V3 Float, L.V3 Float)
basisArb = do
  x <- normArb L.V3
  y <- normArb L.V3
  return $ if x == y then (x, L.cross x y) else (x, y)

triArb :: Gen (Triangle (L.V3 Float))
triArb = do
  p0 <- v3Arb L.V3 arbitrary
  (v1, v2) <- basisArb 
  let p1 = p0 + v1
  let p2 = p0 + v2
  return (p0, p1, p2)

arbitraryList :: Gen a -> Gen [a]
arbitraryList arbitrary =
  sized $
    \n -> do
      k <- choose (0, n)
      sequence [ arbitrary | _ <- [1..k] ]

