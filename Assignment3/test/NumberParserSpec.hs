module NumberParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Modifiers
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Parser.Impl

import SubsAst
import Control.Monad

-- prop_simple_int :: NonNegative Int -> Bool
-- prop_simple_int x = ((parse number "fail" (show (getNonNegative x))) == (Right (getNonNegative x)))

newtype BackslashChar = BackslashChar Char deriving (Show, Eq)
instance Arbitrary BackslashChar where
  arbitrary = oneof $ return `map` (BackslashChar `map` ['\'', '\\'])


newtype NormalChar = NormalChar Char deriving (Show, Eq)
instance Arbitrary NormalChar where
  arbitrary = oneof $ return `map` (NormalChar `map` ['n',  't'])


-- Number tests --
prop_pos_number :: NonNegative Int -> Bool
prop_pos_number x = ((parse posNumber "fail" (show (getNonNegative x))) == (Right $ Number $ getNonNegative x))

prop_neg_number :: NonNegative Int -> Bool
prop_neg_number x = ((parse negNumber "fail" ("-" ++ (show (getNonNegative x)))) == (Right $ Number $ (getNonNegative x) * (-1)))

prop_any_number :: Int -> Bool
prop_any_number x = ((parse number "fail" (show x)) == (Right $ Number x))


-- String tests --
prop_backslash_backslash_char :: BackslashChar -> Bool
prop_backslash_backslash_char (BackslashChar c) = ((parse backslash "fail" ('\\':[c])) == Right c)

prop_backslash_normal_char :: NormalChar -> Bool
prop_backslash_normal_char (NormalChar c) = ((parse backslash "fail" ('\\':[c])) == case c of
                                              'n' -> Right '\n'
                                              't' -> Right '\t'
                                              _ -> fail "Backslash followed by invalid letter")

-- TODO: Add property that only printable characters ([32..126]) are allowed
prop_normal_strings :: ASCIIString -> Bool
prop_normal_strings s = (parse stringParser "fail" ("\'" ++ (getASCIIString s) ++ "\'")) == (Right $ String $ getASCIIString s)


spec :: Spec
spec = do
  describe "posNumber" $ do
    prop "parses a single positive number" $ prop_pos_number

  describe "negNumber" $ do
    prop "parse a single negative number" $ prop_neg_number

  describe "number" $ do
    prop "parses negative and positive numbers" $ prop_any_number

  describe "backslash" $ do
    prop "parses a backslash character prefixed by a \\" $ prop_backslash_backslash_char

  describe "backslash" $ do
    prop "parses a normal character prefixed by a \\" $ prop_backslash_normal_char

  describe "stringParser" $ do
    prop "parses ascii-only strings correctly" $ prop_normal_strings