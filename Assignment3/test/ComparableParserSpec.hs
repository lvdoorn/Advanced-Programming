module ComparableParserSpec (spec) where

import Test.Hspec
import Parser.Impl
import SubsAst

spec :: Spec
spec = do
  describe "comparableParser" $ do
    it "parses 3+4, basic comp" $
      (parse parseComparable "" "3+4") `shouldBe`
      (Right (Call "+" [Number 3,Number 4]))

    it "parses x-5+(-6), check for left associativity" $
      (parse parseComparable "" "x-5+(-6)") `shouldBe`
      Right (Call "+" [Call "-" [Var "x",Number 5],Number (-6)])

    it "parses 9+(3-5), comp with parentheses" $
      (parse parseComparable "" "9+(3-5)") `shouldBe`
      Right (Call "+" [Number 9,Call "-" [Number 3,Number 5]])

    it "parses '9 +( x -5 )', whitespaces comp" $
      (parse parseComparable "" "9 +( x -5 )") `shouldBe`
      Right (Call "+" [Number 9,Call "-" [Var "x",Number 5]])

    it "parses 3*4+5*x, comp with term, check precedence" $
      (parse parseComparable "" "3*4+5*x") `shouldBe`
      Right (Call "+" [Call "*" [Number 3,Number 4],Call "*" [Number 5,Var "x"]])

    it "parses 3*(4+5), comp with term with parentheses" $
      (parse parseComparable "" "3*(4+5)") `shouldBe`
      Right (Call "*" [Number 3,Call "+" [Number 4,Number 5]])
