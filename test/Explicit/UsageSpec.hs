module Explicit.UsageSpec where

import Test.Hspec

import Control.Monad.Writer.Lazy

import Explicit.Usage

import Explicit.Example

spec :: Spec
spec = do
  describe "display" $
    it "formats things into user-facing output" $ do
      display (Kleene $ Symbol "a") `shouldBe` "a*"
      display (Kleene . Symbol $ Literal "a") `shouldBe` "[\"a\"...]"
      display (Positive . Symbol $ Meta "a") `shouldBe` "a..."
      display exampleUsage `shouldBe` "\"git\" flags (\"clone\" | \"init\")"

      display flagDef `shouldBe` "flags = \"--help\" | \"--version\""
      display (Usage exampleUsage [flagDef]) `shouldBe` unlines [display exampleUsage, "", display flagDef]

  describe "example program" $
    it "can show help" $ do
      execWriter (run []) `shouldBe` runHeader

      let names = ["file1", "file2"]
      execWriter (run names) `shouldBe` runHeader ++ names
      execWriter (run $ names ++ ["-h"]) `shouldBe` runHeader ++ names ++ ["-h"]

      let msg = ["Usage: \"example\" (flag | [filepaths...])"]
      execWriter (run ["-h"]) `shouldBe` runHeader ++ helpHeader ++ msg
      execWriter (run ["-h", "a"]) `shouldBe` runHeader ++ helpHeader ++ msg

exampleUsage :: Language Alphabet
exampleUsage = lit "git" #- meta "flags" #- (lit "clone" |- lit "init")

flagDef :: MetaDef
flagDef = MetaDef ("flags", lit "--help" |- lit "--version")
