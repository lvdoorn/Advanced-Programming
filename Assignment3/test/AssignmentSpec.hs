module AssignmentSpec (spec) where

import Test.Hspec
import Parser.Impl
import SubsAst

spec :: Spec
spec = do
  describe "parseAssignment" $ do
    it "parses a simple assignment without whitespace" $
      (parse parseAssignment "" "x=5") `shouldBe`
      (Right $ Assign "x" (Number 5))

    it "parses a simple assignment with whitespace" $
      (parse parseAssignment "" "x = 5") `shouldBe`
      (Right $ Assign "x" (Number 5))

    it "parses a composite assignment without whitespace" $
      (parse parseAssignment "" "x=y=5") `shouldBe`
      (Right $ Assign "x" $ Assign "y" $ Number 5)

    it "parses a composite assignment with whitespace" $
      (parse parseAssignment "" "x = y = 5") `shouldBe`
      (Right $ Assign "x" $ Assign "y" $ Number 5)

    it "parses \'<\' in an assignment with correct associativity" $
      (parse parseAssignment "" "x = 5 < 6 < 7") `shouldBe`
      Right (Assign "x" (Call "<" [Call "<" [Number 5,Number 6],Number 7]))

    it "parses an assignment to a variable" $
      (parse parseAssignment "" "x = y") `shouldBe`
      Right (Assign "x" (Var "y"))
