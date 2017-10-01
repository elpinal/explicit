module Explicit.UsageSpec where

import Test.Hspec

import Explicit.Usage

spec :: Spec
spec = do
  describe "toString" $
    it "stringify Language" $
      toString (Kleene (Symbol "a")) `shouldBe` "a*"

  describe "format" $
    it "stringify Language suitably for usage" $ do
      format (Kleene (Symbol $ Literal "a")) `shouldBe` Meta "[\"a\"...]"
      format (Positive (Symbol $ Meta "a")) `shouldBe` Meta "a..."
      format exampleUsage `shouldBe` Meta "\"git\" flags \"clone\" | \"init\""

exampleUsage :: Language Alphabet
exampleUsage = lit "git" #- meta "flags" #- (lit "clone" |- lit "init")
