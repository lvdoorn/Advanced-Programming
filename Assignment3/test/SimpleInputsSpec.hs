module SimpleInputsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Parser.Impl
import SubsAst

prop_single_int :: Int -> Bool
prop_single_int x = ((parseString (show x)) == (Right (Number x)))

prop_ident_valid :: Bool
prop_ident_valid = ((parse parseIdent "" "nEwVar_1") == (Right $ Var "nEwVar_1"))

spec :: Spec
spec = do
  describe "Single number" $ do
    prop "parses a single number" $ prop_single_int

  describe "Parse identifier" $ do
    prop "parses valid identifier" $ prop_ident_valid
