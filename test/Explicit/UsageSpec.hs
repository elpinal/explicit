module Explicit.UsageSpec where

import Test.Hspec

import Explicit.Usage

spec :: Spec
spec = do
  describe "toString" $
    it "stringify Language" $
      toString (Kleene (Symbol "a")) `shouldBe` "a*"

  describe "toUsageString" $
    it "stringify Language suitably for usage" $
      toUsageString (Kleene (Symbol "a")) `shouldBe` "[\"a\"...]"
