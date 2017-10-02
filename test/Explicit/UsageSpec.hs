module Explicit.UsageSpec where

import Test.Hspec

import Explicit.Usage

spec :: Spec
spec =
  describe "display" $
    it "formats things into user-facing output" $ do
      display (Kleene $ Symbol "a") `shouldBe` "a*"
      display (Kleene . Symbol $ Literal "a") `shouldBe` "[\"a\"...]"
      display (Positive . Symbol $ Meta "a") `shouldBe` "a..."
      display exampleUsage `shouldBe` "\"git\" flags (\"clone\" | \"init\")"

      display flagDef `shouldBe` "flags = \"--help\" | \"--version\""
      display (Usage exampleUsage [flagDef]) `shouldBe` unlines [display exampleUsage, "", display flagDef]

exampleUsage :: Language Alphabet
exampleUsage = lit "git" #- meta "flags" #- (lit "clone" |- lit "init")

flagDef :: MetaDef
flagDef = MetaDef ("flags", lit "--help" |- lit "--version")
