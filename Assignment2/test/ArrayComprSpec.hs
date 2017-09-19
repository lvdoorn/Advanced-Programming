module ArrayComprSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SubsInterpreter
import SubsAst

instance Arbitrary Expr where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z1 <- arbitrary
    z2 <- arbitrary
    -- oneof [return $ Number x, return TrueConst, return FalseConst]
    oneof $ return `map` [Number x,
                          TrueConst,
                          FalseConst,
                          Undefined,
                          String y,
                          Var y,
                          Assign y z1,
                          Array [z1],
                          Array [],
                          Comma z1 z2]

spec :: Spec
spec = do
  describe "Simple ACBody" $ do
    it "evaluates a simple ACBody" $ do
      (runExpr (Compr (ACBody (Number 1)))) `shouldBe` (Right (IntVal 1))

  describe "Simple ACIf" $ do
    it "evaluates an AC with if" $ do
      (runExpr (Compr (ACIf TrueConst (ACBody (Number 1))))) `shouldBe` (Right (IntVal 1))

  describe "Simple ACFor" $ do
    it "evaluates an AC with for" $ do
        (runExpr 
          (Comma
          (Assign
            "xs" (Array [Number 0, Number 1, Number 2, Number 3, Number 4, Number 5, Number 6, Number 7, Number 8, Number 9]))
          (Assign
            "squares"
            (Compr
              (ACFor "x" (Var "xs") (ACBody (Call "*" [Var "x",Var "x"])))))))
        `shouldBe`
        (Right
          (ArrayVal [(IntVal 0), (IntVal 1), (IntVal 4), (IntVal 9), (IntVal 16), (IntVal 25), (IntVal 36), (IntVal 49), (IntVal 64), (IntVal 81)]))
