module TermParserSpec (spec) where

import Test.Hspec
import Parser.Impl
import SubsAst

spec :: Spec
spec = do
  describe "termParser" $ do
    it "parses 2*4, basic term" $
      (parse parseTerm "" "2*4") `shouldBe`
      (Right (Call "*" [Number 2,Number 4]))

    it "parses x*5*(-6), check for left associativity" $
      (parse parseTerm "" "x%5*(-6)") `shouldBe`
      Right (Call "*" [Call "%" [Var "x",Number 5],Number (-6)])

    it "parses 9*(x*5), term with parentheses" $
      (parse parseTerm "" "9*(x*5)") `shouldBe`
      Right (Call "*" [Number 9,Call "*" [Var "x",Number 5]])

    it "parses '9 *( x *5 )', whitespaces term " $
      (parse parseTerm "" "9 *( x *5 )") `shouldBe`
      Right (Call "*" [Number 9,Call "*" [Var "x",Number 5]])
