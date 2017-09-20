module EnvSpec (spec) where

import Test.Hspec

import SubsInterpreter

import qualified Data.Map as Map

initContext :: (Map.Map a b, Map.Map c d)
initContext = (Map.empty, Map.empty)

testEnv :: Map.Map [Char] Value
testEnv =  Map.fromList [("x", IntVal 1), ("y", StringVal "myString")]

testContext :: (Map.Map [Char] Value, Map.Map k a)
testContext = (testEnv, Map.empty)

spec :: Spec
spec = do
  describe "PutVar empty context" $ do
     it "inserts a var into empty env" $ do
      (runSubsM (putVar "x" (IntVal 1))) initContext `shouldBe` (Right ((), Map.fromList [("x", IntVal 1)]))

  describe "PutVar context" $ do
     it "inserts a var into env" $ do
      (runSubsM (putVar "z" (StringVal "newVar"))) testContext `shouldBe` (Right ((), Map.fromList [("x",IntVal 1),("y",StringVal "myString"),("z",StringVal "newVar")]))

  describe "PutVar update var" $ do
     it "updates the value of an existing var" $ do
      (runSubsM (putVar "x" (ArrayVal [IntVal 2]))) testContext `shouldBe` (Right ((), Map.fromList [("x",ArrayVal [IntVal 2]),("y",StringVal "myString")]))

  describe "GetVar" $ do
     it "gets var from the env" $ do
      (runSubsM (getVar "x")) testContext `shouldBe` (Right (IntVal 1, Map.fromList [("x",IntVal 1),("y",StringVal "myString")]))

  describe "GetVar empty env" $ do
     it "gets var from the env" $ do
      (runSubsM (getVar "x")) initContext `shouldBe` (Left "Var not found")

  describe "ModifyEnv with new env" $ do
     it "replaces existing env with a given env" $ do
      (runSubsM (modifyEnv (\_ -> Map.fromList [("z",IntVal 3),("y",StringVal "newString")]))) testContext `shouldBe` (Right ((),Map.fromList [("y",StringVal "newString"),("z",IntVal 3)]))

  describe "ModifyEnv" $ do
     it "returns the same env" $ do
      (runSubsM (modifyEnv (\x -> x))) testContext `shouldBe` (Right ((), testEnv))

  describe "ModifyEnv with empty env" $ do
     it "returns empty env" $ do
      (runSubsM (modifyEnv (\_ -> Map.empty))) testContext `shouldBe` (Right ((),Map.fromList []))