module ArraySpec where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.LLVM.PTX as GPU

import Test.Hspec
import Control.Exception (evaluate)

spec :: Spec
spec = do
  let runExp :: Elt e => Exp e -> e
      runExp e = indexArray (GPU.run (unit e)) Z
  let mat = fromList (Z:.5:.10) [0..] :: Matrix Int

  describe "multi array indexing" $ do
    describe "in bounds" $ do
      let p = (Z:.1:.2)
      it "pulls" $ do
        (runExp $ use mat ! constant p) `shouldBe` 12

    describe "out bounds" $ do
      let p = (Z:.6:.2)
      it "throws" $ do
        (evaluate $ runExp $ use mat ! constant p) `shouldThrow` anyException
  
  describe "single dim indexing" $ do
    describe "in bounds" $ do
      let p = 5 
      it "pulls" $ do
        (runExp $ use mat A.!! constant p) `shouldBe` 5

    -- describe "out bounds" $ do
    --   let p = 51 
    --   it "throws" $ do
    --     print $ runExp $ use mat A.!! constant p
 