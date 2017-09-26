module TermParserSpec (spec) where

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
  describe "termParser" $ do
    it "parses 2*4, basic ter" $
      (parse parseTerm "" "2*4") `shouldBe`
      (Right (Call "*" [Number 2,Number 4]))

    it "parses x*5*(-6), check for left associativity" $
      (parse parseTerm "" "x%5*(-6)") `shouldBe`
      Right (Call "*" [Call "%" [Var "x",Number 5],Number (-6)])


