module NumberParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Modifiers
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Parser.Impl

import SubsAst

-- TODO: create int type between -99999999 and 99999999
-- newtype SubScriptInt = SubScriptInt Int
-- instance Arbitrary SubScriptInt where
--   arbitrary = ...

-- Number tests --
prop_pos_number :: NonNegative Int -> Bool
prop_pos_number x = ((parse posNumber "fail" (show (getNonNegative x))) == (Right $ Number $ getNonNegative x))

prop_neg_number :: NonNegative Int -> Bool
prop_neg_number x = ((parse negNumber "fail" ("-" ++ (show (getNonNegative x)))) == (Right $ Number $ (getNonNegative x) * (-1)))

prop_any_number :: Int -> Bool
prop_any_number x = ((parse number "fail" (show x)) == (Right $ Number x))


spec :: Spec
spec = do
  describe "posNumber" $ do
    prop "parses a single positive number" $ prop_pos_number

  describe "negNumber" $ do
    prop "parse a single negative number" $ prop_neg_number

  describe "number" $ do
    prop "parses negative and positive numbers" $ prop_any_number
