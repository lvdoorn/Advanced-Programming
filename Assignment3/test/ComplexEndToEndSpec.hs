module ComplexEndToEndSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Modifiers
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Parser.Impl

import Data.Char

import SubsAst
import Control.Monad

spec :: Spec
spec = do
  describe "parseString" $ do
    it "parses a composite assignment" $
      parseString "a = b = undefined" `shouldBe`
      (Right (Assign "a" (Assign "b" Undefined)))
