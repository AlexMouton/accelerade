module ArbysSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property

import Arbys

spec :: Spec
spec = do
    describe "arbFloatUnit" $ do
      it "stays between 0 and 1" $ property $ do 
        f <- arbFloatUnit
        return $ if (f >= 0.0 && f <= 1.0)
          then succeeded
          else failed