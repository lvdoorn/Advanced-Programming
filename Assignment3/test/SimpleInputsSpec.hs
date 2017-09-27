module SimpleInputsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Parser.Impl

import Data.Char
import Data.Either
import SubsAst

newtype IdentGen = IdentGen [Char] deriving (Eq, Show)

instance Arbitrary IdentGen where
    arbitrary = fmap IdentGen (listOf $ elements (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"))

newtype KeywordGen = KeywordGen [Char] deriving (Eq, Show)

instance Arbitrary KeywordGen where
    arbitrary = fmap KeywordGen (elements keywordsForTest)

keywordsForTest :: [String]
keywordsForTest = ["true", "false", "undefined", "for", "of", "if"]

prop_single_int :: Int -> Bool
prop_single_int x = ((parseString (show x)) == (Right (Number x)))

prop_ident_valid :: IdentGen -> Property
prop_ident_valid (IdentGen x) = (x /= "" && isLetter (head x)) ==> ((parse parseIdent "" x) == (Right $ Var x))

prop_ident_keyword :: KeywordGen -> Bool
prop_ident_keyword (KeywordGen x) = isLeft (parse parseIdent "" x)

prop_ident_invalid :: IdentGen -> Property
prop_ident_invalid (IdentGen x) = (x /= "" && (not $ isLetter (head x))) ==> (isLeft (parse parseIdent "" x))

spec :: Spec
spec = do
  describe "Single number" $ do
    prop "parses a single number" $ prop_single_int

  describe "Parse identifier" $ do
    prop "parses valid identifier" $ prop_ident_valid
    prop "parses keyword as identifier" $ prop_ident_keyword
    prop "parses invalid identifier" $ prop_ident_invalid
  