module NumberParserSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Parser.Impl
import Control.Monad

import Data.Either
import SubsAst

newtype PositiveIntGen = PositiveIntGen Int deriving (Eq, Show)

instance Arbitrary PositiveIntGen where
    arbitrary = PositiveIntGen `liftM` (choose (0, 99999999))

newtype IntGen = IntGen Int deriving (Eq, Show)

instance Arbitrary IntGen where
    arbitrary = IntGen `liftM` (choose (-99999999, 99999999))

-- Number tests --
prop_pos_number :: PositiveIntGen -> Bool
prop_pos_number (PositiveIntGen x) = ((parse posNumber "fail" (show (x))) == (Right $ Number $ x))

prop_neg_number :: PositiveIntGen -> Bool
prop_neg_number (PositiveIntGen x) = ((parse negNumber "fail" ("-" ++ (show (x)))) == (Right $ Number $ (x) * (-1)))

prop_any_number :: IntGen -> Bool
prop_any_number (IntGen x) = ((parse number "fail" (show x)) == (Right $ Number x))

spec :: Spec
spec = do
  describe "posNumber" $ do
    prop "parses a single positive number" $ prop_pos_number

    it "fails to parse a number with more than 8 digits" $
      isLeft (parse number "" "123456789")

  describe "negNumber" $ do
    prop "parses a single negative number" $ prop_neg_number

    it "fails to parse a number with more than 8 digits" $
      isLeft (parse number "" "-123456789")

  describe "number" $ do
    prop "parses negative and positive numbers" $ prop_any_number

    it "parses a number followed by whitespace" $
      (parse number "fail" "3      ") `shouldBe`
      (Right $ Number 3)

    it "fails to parse a positive double" $
      isLeft (parse number "fail" "1.5")

    it "fails to parse a negative double" $
      isLeft (parse number "fail" "-1.5")

    it "fails to parse a big positive number" $
      isLeft (parse number "fail" "111111111")

    it "fails to parse a big negative number" $
      isLeft (parse number "fail" "-222222222")

    it "fails to parse a single minus" $
      isLeft (parse number "fail" "-")

    it "fails to parse a double negative" $
      isLeft (parse number "fail" "--2")