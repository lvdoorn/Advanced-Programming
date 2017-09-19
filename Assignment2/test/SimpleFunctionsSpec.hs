module SimpleFunctionsSpec (spec) where

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
prop_number :: Int -> Bool
prop_number x = runExpr (Number x) == Right (IntVal x)

prop_string :: String -> Bool
prop_string s = runExpr (String s) == Right (StringVal s)

prop_empty_array :: Bool
prop_empty_array = runExpr (Array []) == Right (ArrayVal [])

prop_undefined :: Bool
prop_undefined = runExpr (Undefined) == Right (UndefinedVal)

prop_true :: Bool
prop_true = runExpr (TrueConst) == Right (TrueVal)

prop_false :: Bool
prop_false = runExpr (FalseConst) == Right (FalseVal)

prop_var :: Bool
prop_var = runExpr (Var "x") == Left "Var not found"

prop_assign :: Expr -> Bool
prop_assign expr = runExpr (Assign "x" expr) == runExpr expr

-- prop_comma :: Expr -> Expr -> Bool -- probably not be correct, e2 could give error on 
--                                    -- looking up a var added by e1
-- prop_comma e1 e2 = runExpr (Comma e1 e2) == 

spec :: Spec
spec = do
  describe "Unit number" $ do
    prop "returns an IntVal" $ prop_number
  
  describe "Unit string" $ do
    prop "returns a StringVal" $ prop_string

  describe "Empty array" $ do
    prop "returns an empty array" $ prop_empty_array

  describe "Unit Undefined" $ do
    prop "returns undefined" $ prop_undefined 

  describe "Unit true" $ do
    prop "returns true" $ prop_true

  describe "Unit false" $ do
    prop "returns false" $ prop_false

  describe "Undef var" $ do
    prop "return error" $ prop_var

  describe "Assign var" $ do
    prop "returns assigned value" $ prop_assign

  -- describe "Comma" $ do
  --   prop "returns the value of the second expression" $ prop_comma