module Explicit.UsageSpec where

import Test.Hspec

import Explicit.Usage

spec :: Spec
spec = do
  describe "toString" $
    it "stringify Language" $
      toString (Kleene $ Symbol "a") `shouldBe` "a*"

  describe "format" $
    it "stringify Language suitably for usage" $ do
      format (Kleene . Symbol $ Literal "a") `shouldBe` Meta "[\"a\"...]"
      format (Positive . Symbol $ Meta "a") `shouldBe` Meta "a..."
      format exampleUsage `shouldBe` Meta "\"git\" flags (\"clone\" | \"init\")"

  describe "MetaDef" $
    it "is the definition of a meta variable" $
      display (MetaDef ("flags", lit "--help" |- lit "--version")) `shouldBe` "flags = \"--help\" | \"--version\""

  describe "Usage" $
    it "can be formatted" $
      display (Usage exampleUsage [flagDef]) `shouldBe` unlines [display (format exampleUsage), "", display flagDef]

exampleUsage :: Language Alphabet
exampleUsage = lit "git" #- meta "flags" #- (lit "clone" |- lit "init")

flagDef :: MetaDef
flagDef = MetaDef ("flags", lit "--help" |- lit "--version")
