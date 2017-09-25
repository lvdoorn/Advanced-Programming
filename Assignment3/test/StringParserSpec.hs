module StringParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Modifiers
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Parser.Impl

import Data.Char

import SubsAst
import Control.Monad

newtype BackslashChar = BackslashChar Char deriving (Show, Eq)
instance Arbitrary BackslashChar where
  arbitrary = oneof $ return `map` (BackslashChar `map` ['\'', '\\'])


newtype NormalChar = NormalChar Char deriving (Show, Eq)
instance Arbitrary NormalChar where
  arbitrary = oneof $ return `map` (NormalChar `map` ['n',  't'])

-- String tests --
prop_backslash_backslash_char :: BackslashChar -> Bool
prop_backslash_backslash_char (BackslashChar c) = ((parse backslash "fail" ('\\':[c])) == Right c)

prop_backslash_normal_char :: NormalChar -> Bool
prop_backslash_normal_char (NormalChar c) = ((parse backslash "fail" ('\\':[c])) == case c of
                                              'n' -> Right '\n'
                                              't' -> Right '\t'
                                              _ -> fail "Backslash followed by invalid letter")

-- TODO: Add generator for proper strings
prop_normal_strings :: ASCIIString -> Property
prop_normal_strings s = let str = getASCIIString s in
    all (\x -> ord x >= 32 && ord x <= 126 && x /= '\\' && x /= '\'') str
      ==> (parse stringParser "fail" ("\'" ++ (getASCIIString s) ++ "\'")) ==
          (Right $ String $ getASCIIString s)

spec :: Spec
spec = do
  describe "backslash" $ do
    prop "parses a backslash character prefixed by a \\" $ prop_backslash_backslash_char

  describe "backslash" $ do
    prop "parses a normal character prefixed by a \\" $ prop_backslash_normal_char

  describe "stringParser" $ do
    prop "parses ascii-only strings correctly" $ prop_normal_strings