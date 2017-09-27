module StringParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Modifiers
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Parser.Impl

import Data.Char
import Data.Either (isLeft)

import SubsAst

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

    prop "parses a normal character prefixed by a \\" $ prop_backslash_normal_char

  describe "parseChar" $ do
    it "parses a single character" $
      (parse parseChar "fail" "a") `shouldBe`
      Right 'a'

    it "parses \\n" $
      (parse parseChar "fail" "\\n") `shouldBe`
      Right '\n'

    it "parses \\t" $
      (parse parseChar "fail" "\\t") `shouldBe`
      Right '\t'

    it "parses \\\'" $
      (parse parseChar "fail" "\\\'") `shouldBe`
      Right '\''

    it "parses \\\\" $
      (parse parseChar "fail" "\\\\") `shouldBe`
      Right '\\'

    it "parses a\\<newline>" $
      (parse parseChar "fail" "a\\\n") `shouldBe`
      Right 'a'

  describe "stringParser" $ do
    prop "parses ascii-only strings" $ prop_normal_strings

    it "fails to parse a single backslash (\'\\\')" $
      isLeft (parse stringParser "fail" "\'\\\'")

    it "parses an empty string" $
      (parse stringParser "fail" "\'\'") `shouldBe`
      (Right $ String "")

    it "fails to parse a single newline (\'<newline>\')" $
      isLeft (parse stringParser "fail" "\'\n\'")

    it "fails to parse a single tab (\'<tab>\')" $
      isLeft (parse stringParser "fail" "\'\t\'")

    it "parses a string with a single quote" $
      (parse stringParser "fail" "\'\\\'\'") `shouldBe`
      (Right $ String "\'")

    it "parses a string with a single character" $
      (parse stringParser "fail" "\'a\'") `shouldBe`
      (Right $ String "a")

    it "ignores newlines" $
      (parse stringParser "fail" "\'\\\ntest\\\ning\\\n\'") `shouldBe`
      (Right $ String "testing")

    it "parses escape sequences correctly" $
      parseString "\'\\\'\\n\\t\\\\\'" `shouldBe`
      (Right $ String "\'\n\t\\")

    it "parses a\\<newline>" $
      (parse stringParser "fail" "\'a\\\n\'") `shouldBe`
      (Right $ String "a")