module Main where

import Test.Hspec

import qualified TriPointSpec

main :: IO ()
main = hspec $ do
  describe "TriPoint" TriPointSpec.spec
