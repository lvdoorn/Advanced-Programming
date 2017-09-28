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
  -- describe "Simple ACBody" $ do
  --   it "evaluates a simple ACBody" $ do
  --     (runExpr (Compr (ACBody (Number 1)))) `shouldBe` (Right (IntVal 1))

  -- describe "Simple ACIf true" $ do
  --   it "evaluates an AC with if" $ do
  --     (runExpr (Compr (ACIf TrueConst (ACBody (Number 1))))) `shouldBe` (Right (IntVal 1))

  describe "Only evens" $ do
    it "generates even number 0-8" $ do
      (runExpr (Comma (Assign "xs"
                  (Array [Number 0, Number 1, Number 2, Number 3, Number 4,
                    Number 5, Number 6, Number 7, Number 8, Number 9]))(Compr
                  (ACFor "x" (Var "xs")
                        (ACIf (Call "===" [Call "%" [Var "x", Number 2],
                          Number 0])
                        (ACBody (Var "x")))))))
      `shouldBe`
        Right (ArrayVal [IntVal 0, IntVal 2,IntVal 4,IntVal 6,IntVal 8])
          

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
  
  describe "Scope test" $ do
    it "evaluates the scope example" $ do
      (runExpr (Comma (Assign "x" (Number 42))
                (Comma (Assign "y" (Compr (ACFor "x" (Array [Number 1, Number 2, Number 3])
                                          (ACBody (Var "x")))))
                  (Array [Var "x", Var "y"]))))
      `shouldBe`
      (Right (ArrayVal [IntVal 42, ArrayVal [IntVal 1, IntVal 2, IntVal 3]]))

  describe "Nested for" $ do
    it "evaluates a nested for" $ do
      (runExpr $ Comma (Assign "xs"
                        (Array [Number 1,Number 2,Number 3]))
                       (Compr (ACFor "x"
                                     (Var "xs")
                                     (ACFor "y" (Var "xs") (ACBody (Number 0))))))
      `shouldBe`
      (Right (ArrayVal [IntVal 0,IntVal 0,IntVal 0,IntVal 0,IntVal 0,IntVal 0,IntVal 0,IntVal 0,IntVal 0]))

  describe "Scope test 2" $ do
    it "evaluates a scope example" $
      (runExpr $ Comma (Assign "x" (Number 0))
                       (Comma (Compr (ACFor "y"
                                     (Array [Number 1,Number 2,Number 3])
                                     (ACBody (Assign "x" (Var "y")))))
                       (Var "x")))
      `shouldBe` Right (IntVal 3)