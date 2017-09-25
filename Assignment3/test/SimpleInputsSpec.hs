module SimpleInputsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Parser.Impl

import Data.Char

import SubsAst

newtype IdentGen = IdentGen [Char] deriving (Eq, Show)

instance Arbitrary IdentGen where
    arbitrary = fmap IdentGen (listOf $ elements (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"))

prop_single_int :: Int -> Bool
prop_single_int x = ((parseString (show x)) == (Right (Number x)))

prop_ident_valid :: IdentGen -> Property
prop_ident_valid (IdentGen x) = (x /= "" && isLetter (head x)) ==> ((parse parseIdent "" x) == (Right $ Var x))

spec :: Spec
spec = do
  describe "Single number" $ do
    prop "parses a single number" $ prop_single_int

  describe "Parse identifier" $ do
    prop "parses valid identifier" $ prop_ident_valid