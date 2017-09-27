module FactorParserSpec (spec) where

import Test.Hspec
import Parser.Impl
import SubsAst

spec :: Spec
spec = do
  describe "parseFactor" $ do
    it "parses \"true\"" $
      (parse parseFactor "" "true") `shouldBe`
      (Right TrueConst)

    it "parses \"false\"" $
      (parse parseFactor "" "false") `shouldBe`
      (Right FalseConst)

    it "parses \"undefined\"" $
      (parse parseFactor "" "undefined") `shouldBe`
      (Right Undefined)

    it "parses a normal string" $
      (parse parseFactor "" "\'abcdefg123456\'") `shouldBe`
      (Right $ String "abcdefg123456")

    it "parses a variable" $
      (parse parseFactor "" "x") `shouldBe`
      (Right $ Var "x")

    it "parses an expression in parentheses" $
      (parse parseFactor "" "(1)") `shouldBe`
      (Right $ Number 1)

    it "parses a multiline string" $
      (parse parseFactor "" "\'foo\\\nbar\'") `shouldBe`-- 'foo\
                                                         --   bar'
      (Right $ String "foobar")

    it "parses a variable like a keyword" $
      (parse parseFactor "" "truex") `shouldBe`
      (Right $ Var "truex")

    it "parses an expression in parentheses with whitespace" $
      (parse parseFactor "" "(     5   )   ") `shouldBe`
      (Right $ Number 5)
