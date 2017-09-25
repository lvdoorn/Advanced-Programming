module FactorParserSpec (spec) where

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
  describe "factorParser" $ do
    it "parses \"true\"" $
      (parse factorParser "" "true") `shouldBe`
      (Right TrueConst)

    it "parses \"false\"" $
      (parse factorParser "" "false") `shouldBe`
      (Right FalseConst)

    it "parses \"undefined\"" $
      (parse factorParser "" "undefined") `shouldBe`
      (Right Undefined)

    it "parses a normal string" $
      (parse factorParser "" "\'abcdefg123456\'") `shouldBe`
      (Right $ String "abcdefg123456")

    it "parses a variable" $
      (parse factorParser "" "x") `shouldBe`
      (Right $ Var "x")

    it "parses an expression in parentheses" $
      (parse factorParser "" "(1)") `shouldBe`
      (Right $ Number 1)

    it "parses a multiline string" $
      (parse factorParser "" "\'foo\\\nbar\'") `shouldBe`-- 'foo\
                                                         --   bar'
      (Right $ String "foobar")