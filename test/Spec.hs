module Main where
import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

import Test.Hspec

import qualified DotpSpec
import qualified TriPointSpec
import qualified TriPointAccSpec
import qualified ArraySpec
import qualified ArbysSpec

main :: IO ()
main = hspec $ do
  describe "Arbys" ArbysSpec.spec
  describe "Dotp" DotpSpec.spec
  describe "TriPoint"  TriPointSpec.spec
  describe "TriPointAcc"  TriPointAccSpec.spec
  describe "Array"  ArraySpec.spec
