module NumberParserSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Parser.Impl
import SubsAst


prop_simple_int :: Int -> Bool
prop_simple_int x = ((parseString (show x)) == (Right (Number x)))

spec :: Spec
spec = do
  describe "Single number" $ do
    prop "parses a single number" $ prop_simple_int