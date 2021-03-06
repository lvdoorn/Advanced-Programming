module OperatorFreeSpec (spec) where

import Test.Hspec
import Parser.Impl
import SubsAst

spec :: Spec
spec = do
  describe "parseString" $ do
    it "parses the true constant" $
      (parseString "true") `shouldBe`
      (Right TrueConst)

    it "parses the false constant" $
      (parseString "false") `shouldBe`
      (Right FalseConst)

    it "parses undefined" $
      (parseString "undefined") `shouldBe`
      (Right Undefined)

    it "parses a single variable" $
      (parseString "x") `shouldBe`
      (Right $ Var "x")

    it "parses an empty array" $
      (parseString "[]") `shouldBe`
      (Right $ Array [])

    it "parses an array with one element" $
      (parseString "[1]") `shouldBe`
      (Right $ Array [Number 1])

    it "parses an array of expressions" $
      (parseString "[1, 2, 3]") `shouldBe`
      (Right $ Array [Number 1, Number 2, Number 3])

    it "parses a simple array comprehension" $
      (parseString "[for(x of 1)2]") `shouldBe`
      (Right $ (Compr (ACFor "x" (Number 1) (ACBody (Number 2)))))

    it "parses an array comprehension with if" $
      (parseString "[for (x of 1) if (true) 3]") `shouldBe`
      (Right $ Compr $ ACFor "x" (Number 1) (ACIf TrueConst (ACBody $ Number 3)))

    it "parses a nested array comprehension" $
      (parseString "[for (x of 1) for (y of 2) 3]") `shouldBe`
      (Right $ Compr $ ACFor "x" (Number 1) (ACFor "y" (Number 2) (ACBody $ Number 3)))

    it "parses a simple comma expression" $
      (parseString "1, 2") `shouldBe`
      (Right $ Comma (Number 1) (Number 2))

    it "parses a composite comma expression" $
      (parseString "1, 2, 3") `shouldBe`
      (Right $ Comma (Number 1) (Comma (Number 2) (Number 3)))
