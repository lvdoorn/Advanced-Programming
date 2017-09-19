module PrimitivesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SubsInterpreter
import SubsAst

prop_plus_int_int :: Int -> Int -> Bool
prop_plus_int_int x y = runExpr (Call "+" [(Number x), (Number y)]) == Right (IntVal (x + y))

prop_plus_str_str :: String -> String -> Bool
prop_plus_str_str x y = runExpr (Call "+" [(String x), (String y)]) == Right (StringVal (x ++ y))

prop_plus_str_int :: String -> Int -> Bool
prop_plus_str_int x y = runExpr (Call "+" [(String x), (Number y)]) == Right (StringVal (x ++ show y))

prop_plus__int_str :: Int -> String -> Bool
prop_plus__int_str x y = runExpr (Call "+" [(Number x), (String y)]) == Right (StringVal (show x ++ y))

prop_plus_err_type :: Int -> Bool
prop_plus_err_type x = runExpr (Call "+" [(TrueConst), (Number x)]) == Left "(+) called with wrong number or type of arguments"

prop_plus_err_amount :: Int -> Int -> Int -> Bool
prop_plus_err_amount x y z = runExpr (Call "+" [(Number x), (Number y), (Number z)]) == Left "(+) called with wrong number or type of arguments"

prop_minus_correct :: Int -> Int -> Bool
prop_minus_correct x y = runExpr (Call "-" [Number x, Number y]) == Right (IntVal (x - y))

prop_minus_err_type :: Int -> Bool
prop_minus_err_type x = runExpr (Call "-" [(TrueConst), (Number x)]) == Left "(-) called with wrong number or type of arguments"

prop_minus_err_amount :: Int -> Int -> Int -> Bool
prop_minus_err_amount x y z = runExpr (Call "-" [(Number x), (Number y), (Number z)]) == Left "(-) called with wrong number or type of arguments"

prop_product_correct :: Int -> Int -> Bool
prop_product_correct x y = runExpr (Call "*" [Number x, Number y]) == Right (IntVal (x * y))

prop_product_err_type :: Int -> Bool
prop_product_err_type x = runExpr (Call "*" [(TrueConst), (Number x)]) == Left "(*) called with wrong number or type of arguments"

prop_product_err_amount :: Int -> Int -> Int -> Bool
prop_product_err_amount x y z = runExpr (Call "*" [(Number x), (Number y), (Number z)]) == Left "(*) called with wrong number or type of arguments"

prop_mod_correct :: Int -> Int -> Bool
prop_mod_correct x y = runExpr (Call "%" [Number x, Number y]) == Right (IntVal (x `mod` y))

prop_mod_err_type :: Int -> Bool
prop_mod_err_type x = runExpr (Call "%" [(TrueConst), (Number x)]) == Left "(%) called with wrong number or type of arguments"

prop_mod_err_amount :: Int -> Int -> Int -> Bool
prop_mod_err_amount x y z = runExpr (Call "%" [(Number x), (Number y), (Number z)]) == Left "(%) called with wrong number or type of arguments"

prop_less_than_correct_true :: Int -> Int -> Property
prop_less_than_correct_true x y = (x < y) ==> (runExpr (Call "<" [Number x, Number y]) == Right TrueVal)

prop_less_than_correct_false :: Int -> Int -> Property
prop_less_than_correct_false x y = (x >= y) ==> (runExpr (Call "<" [Number x, Number y]) == Right FalseVal)

prop_less_than_err_type :: Int -> Bool
prop_less_than_err_type x = runExpr (Call "<" [TrueConst, Number x]) == Left "(<) called with wrong number or type of arguments"

prop_less_than_err_amount :: Int -> Int -> Int -> Bool
prop_less_than_err_amount x y z = runExpr (Call "<" [Number x, Number y, Number z]) == Left "(<) called with wrong number or type of arguments"


spec :: Spec
spec = do
  describe "Plus int int" $ do
    prop "returns the sum of the inputs" $ prop_plus_int_int

  describe "Plus str str" $ do
    prop "appends the two input strings" $ prop_plus_str_str

  describe "Plus str int" $ do
    prop "appends an int to a string" $ prop_plus_str_int

  describe "Plus int str" $ do
    prop "appends a string to an int" $ prop_plus__int_str

  describe "Plus wrong type" $ do
    prop "will give an error" $ prop_plus_err_type

  describe "Plus wrong amount args" $ do
    prop "will give an error" $ prop_plus_err_amount

  describe "Minus correct" $ do
    prop "will return a correct result" $ prop_minus_correct

  describe "Minus wrong type" $ do
    prop "will give an error" $ prop_minus_err_type

  describe "Minus wrong amount args" $ do
    prop "will give an error" $ prop_minus_err_amount

  describe "Product correct" $ do
    prop "will return a correct result" $ prop_product_correct

  describe "Product wrong type" $ do
    prop "will give an error" $ prop_product_err_type

  describe "Product wrong amount args" $ do
    prop "will give an error" $ prop_product_err_amount

  describe "Modulo correct" $ do
    prop "will return a correct result" $ prop_mod_correct

  describe "Modulo wrong type" $ do
    prop "will give an error" $ prop_mod_err_type

  describe "Modulo wrong amount args" $ do
    prop "will give an error" $ prop_mod_err_amount

  describe "Less than true case" $ do
    prop "will return true" $ prop_less_than_correct_true

  describe "Less than false case" $ do
    prop "will return false" $ prop_less_than_correct_false

  describe "Less than wrong type" $ do
    prop "will give an error" $ prop_less_than_err_type

  describe "Less than wrong amount args" $ do
    prop "will give an error" $ prop_less_than_err_amount