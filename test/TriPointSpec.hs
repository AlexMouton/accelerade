module TriPointSpec where
import Test.Hspec

import qualified Linear.V3 as L

import TriPoint

spec :: Spec
spec = do
  describe "tripoint" $ do
    describe "inside" $ do
      let tri = ( L.V3 (1.0 :: Float) (0.0 :: Float) (0.0 :: Float)
                , L.V3 (0.0 :: Float) (1.0 :: Float) (0.0 :: Float)
                , L.V3 (0.0 :: Float) (0.0 :: Float) (0.0 :: Float)
                )
      let point = L.V3 (0.1 :: Float) (0.1 :: Float) (0.0 :: Float)
      it "hits" $ do
        (barycentric tri point) `shouldBe` True

    describe "outside" $ do
      let tri = ( L.V3 (1.0 :: Float) (0.0 :: Float) (0.0 :: Float)
                , L.V3 (0.0 :: Float) (1.0 :: Float) (0.0 :: Float)
                , L.V3 (0.0 :: Float) (0.0 :: Float) (0.0 :: Float)
                )
      let point = L.V3 (1.0 :: Float) (1.0 :: Float) (0.0 :: Float)
      it "misses" $ do
        (barycentric tri point) `shouldBe` False