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

  describe "display" $
    it "formats things into user-facing output" $ do
      display flagDef `shouldBe` "flags = \"--help\" | \"--version\""
      display (Usage exampleUsage [flagDef]) `shouldBe` unlines [display exampleUsage, "", display flagDef]

exampleUsage :: Language Alphabet
exampleUsage = lit "git" #- meta "flags" #- (lit "clone" |- lit "init")

flagDef :: MetaDef
flagDef = MetaDef ("flags", lit "--help" |- lit "--version")
