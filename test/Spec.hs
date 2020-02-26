import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

import Test.Hspec
import System.TimeIt (timeItNamed)

import qualified DotpSpec
import qualified TriPointSpec
import qualified ArraySpec

main :: IO ()
main = hspec $ do
  -- describe "Dotp" DotpSpec.spec
  describe "TriPoint"  TriPointSpec.spec
  describe "Array"  ArraySpec.spec
