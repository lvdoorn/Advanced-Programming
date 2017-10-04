module ComplexEndToEndSpec (spec) where

import Test.Hspec
import Parser.Impl
import SubsAst

spec :: Spec
spec = do
  describe "parseString" $ do
    it "parses a composite assignment" $
      parseString "a = b = undefined" `shouldBe`
      (Right (Assign "a" (Assign "b" Undefined)))
    
    it "parses scope.js expr" $
       parseString "x = 42, y = [for (x of 'abc') x], [x, y]" `shouldBe`
       (Right (Comma (Assign "x" (Number 42)) (Comma (Assign "y" (Compr (ACFor "x" (String "abc") (ACBody (Var "x"))))) (Array [Var "x", Var "y"])))) 
    
    it "parses intro.js expr" $
       parseString "xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],squares = [ for (x of xs) x * x ],evens = [ for (x of xs) if (x % 2 === 0) x ],many_a = [ for (x of xs) for (y of xs) 'a' ],hundred = [ for (i of [0]) for (x of Array(5)) for (y of Array(20)) i = i + 1 ],[xs, squares, evens, many_a, hundred]" `shouldBe`
      (Right (Comma (Assign "xs" (Array [Number 0, Number 1, Number 2, Number 3, Number 4, Number 5, Number 6, Number 7, Number 8, Number 9])) (Comma (Assign "squares" (Compr (ACFor "x" (Var "xs") (ACBody (Call "*" [Var "x",Var "x"]))))) (Comma (Assign "evens" (Compr (ACFor "x" (Var "xs") (ACIf (Call "===" [Call "%" [Var "x", Number 2], Number 0]) (ACBody (Var "x")))))) (Comma (Assign "many_a" (Compr (ACFor "x" (Var "xs") (ACFor "y" (Var "xs") (ACBody (String "a")))))) (Comma (Assign "hundred" (Compr (ACFor "i" (Array [Number 0]) (ACFor "x" (Call "Array" [Number 5]) (ACFor "y" (Call "Array" [Number 20]) (ACBody (Assign "i" (Call "+" [Var "i", Number 1])))))))) (Array [Var "xs", Var "squares", Var "evens", Var "many_a", Var "hundred"])))))))
    
    it "parses function calls with correct precedence" $
      parseString "f(3) * f(4)" `shouldBe`
      (Right (Call "*" [(Call "f" [Number 3]), (Call "f" [Number 4])]))

    it "parses function calls with longer function names with correct precedence" $
      parseString "func   (3) * func2  (4)" `shouldBe`
      (Right (Call "*" [(Call "func" [Number 3]), (Call "func2" [Number 4])]))

    it "parses an array expression with correct precedence" $
      parseString "[1, 2, 3] + [4, 5, 6]" `shouldBe`
      (Right (Call "+" [(Array [Number 1, Number 2, Number 3]), (Array [Number 4, Number 5, Number 6])]))

    it "parses an array comprehension with correct precedence" $
      parseString "[for (a of abc) 1] * [for (b of abc) 2]" `shouldBe`
      (Right (Call "*" [(Compr (ACFor "a" (Var "abc") (ACBody (Number 1)))), (Compr (ACFor "b" (Var "abc") (ACBody (Number 2))))]))