module AssignableParserSpec (spec) where

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
  describe "assignableParser" $ do
    it "parses 1<2, basic assignable" $
      (parse parseAssignable "" "1<2") `shouldBe`
      (Right (Call "<" [Number 1,Number 2]))

    it "parses true===true, basic assignable" $
      (parse parseAssignable "" "true===true") `shouldBe`
      Right (Call "===" [TrueConst,TrueConst])

    it "parses 3*2<4+5, check precedence" $
      (parse parseAssignable "" "3*2<4+5") `shouldBe`
      Right (Call "<" [Call "*" [Number 3,Number 2],Call "+" [Number 4,Number 5]])

    it "parses 3+x===4, check precedence" $
      (parse parseAssignable "" "3+x===4") `shouldBe`
      Right (Call "===" [Call "+" [Number 3,Var "x"],Number 4])

    it "parses false===(5<4), assignable with parentheses" $
      (parse parseAssignable "" "false===(5<4)") `shouldBe`
      Right (Call "===" [FalseConst,Call "<" [Number 5,Number 4]])


    it "parses \'<\' with correct associativity" $
      (parse parseAssignable "" "5 < 6 < 7") `shouldBe`
      Right (Call "<" [Call "<" [Number 5,Number 6],Number 7])
