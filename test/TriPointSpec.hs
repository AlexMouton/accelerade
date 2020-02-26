module TriPointSpec where
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property

import Debug.Trace

import qualified Linear.V3 as L
import Data.Fixed (mod')
import TriPoint

arbFloatPos :: Gen Float
arbFloatPos = abs <$> arbitrary

arbFloatPosEps :: Gen Float
arbFloatPosEps = ((+) (0.01 :: Float)) <$> arbFloatPos 

arbFloatUnit :: Gen Float
arbFloatUnit = mod1 <$> arbitrary
  where
    mod1 :: Float  -> Float
    mod1 x = (abs x) `mod'` 1.0

v3Arb :: Gen Float -> Gen (L.V3 Float)
v3Arb f = L.V3 <$> f <*> f <*> f 

normArb :: Gen (L.V3 Float)
normArb = do 
  phi <- (*) pi <$> arbFloatUnit
  theta <- (*) (2.0 * pi ) <$> arbFloatUnit
  let x = sin phi * cos theta
  let y = sin phi * sin theta 
  let z = cos phi
  return $ L.V3 x y z

baryArb :: Gen (L.V3 Float)
baryArb = do
  x <- arbFloatUnit
  y <- (*) (1.0 - x) <$> arbFloatUnit
  let z = 1.0 - (x + y)
  return $ L.V3 x y z

basisArb :: Gen (L.V3 Float, L.V3 Float)
basisArb = do
  x <- normArb
  y <- normArb
  return $ if x == y then (x, L.cross x y) else (x, y)

triArb :: Gen (Triangle Float)
triArb = do
  p0 <- v3Arb arbitrary
  (v1, v2) <- basisArb 
  let p1 = p0 + v1
  let p2 = p0 + v2
  return (p0, p1, p2)


spec :: Spec
spec = do
  describe "tripoint" $ do
    describe "arbFloatUnit" $ do
      it "stays between 0 and 1" $ property $ do 
        f <- arbFloatUnit
        return $ if (f >= 0.0 && f <= 1.0)
          then succeeded
          else failed

    describe "barycentric" $ do
      describe "inside" $ do
        it "hits" $ property $ do
          p0 <- v3Arb arbitrary
          (v1, v2) <- basisArb 
          let p1 = p0 + v1
          let p2 = p0 + v2
          let tri = ( p0 
                    , p1
                    , p2
                    )

          (L.V3 b0 b1 b2) <- baryArb

          let point = p0 * pure b0 + p1 * pure b1 + p2 * pure  b2
          let res = barycentric tri point
          -- log res 
          return $ if res == True 
            then succeeded 
            else failed

      describe "outside" $ do
        let tri = ( L.V3 (1.0 :: Float) (0.0 :: Float) (0.0 :: Float)
                  , L.V3 (0.0 :: Float) (1.0 :: Float) (0.0 :: Float)
                  , L.V3 (0.0 :: Float) (0.0 :: Float) (0.0 :: Float)
                  )
        let point = L.V3 (1.0 :: Float) (1.0 :: Float) (0.0 :: Float)
        it "misses" $ do
          (barycentric tri point) `shouldBe` False