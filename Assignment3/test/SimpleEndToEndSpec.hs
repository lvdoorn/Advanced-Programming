module SimpleEndToEndSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Modifiers
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Parser.Impl

import Data.Char

import SubsAst
import Control.Monad





spec :: Spec
spec = do
  describe "parseString" $ do
    it "parses a number surrounded by whitespace" $
      (parseString "     3 \t\t\t      \t\t\t ") `shouldBe`
      (Right $ Number 3)

    it "parses whitespace in a string constant" $
      (parseString "'     asdf       234     '") `shouldBe`
      (Right $ String "     asdf       234     ")

    it "parses a multiline string" $
      (parseString "'abc\\\ndef'") `shouldBe`
      (Right $ String "abcdef")

    it "parses a comment" $
      (parseString "123 // this is a comment") `shouldBe`
      (Right $ Number 123)

    it "parses a string with a pseudocomment" $
      (parseString "'This is a string // this is not a comment'") `shouldBe`
      (Right $ String "This is a string // this is not a comment")