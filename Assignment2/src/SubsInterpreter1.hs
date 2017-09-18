module SubsInterpreter
       (
         Value(..)
       , runExpr
       )
       where

import SubsAst
import Data.Char 
-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show, Ord)


type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> Either Error Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

env :: Map Ident Value
env = Map.fromList [("x", IntVal 1), ("y", StringVal "myString")]

primitive = mkArray [IntVal 3]

penv :: Map FunName Primitive
penv = Map.fromList [("*", mkArray), ("+", mkArray)]

context = (env, penv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", equalityPrimitive)
                       , ("<", lessThanPrimitive)
                       , ("+", plusPrimitive)
                       , ("*", productPrimitive)
                       , ("-", minusPrimitive)
                       , ("%", modPrimitive)
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

u :: Context -> Either Error (Value, Env)
u ctx = Right (IntVal 1, env)

instance Functor SubsM where
  fmap f = undefined

instance Applicative SubsM where
  pure = undefined
  (<*>) = undefined

instance Monad SubsM where
  return x = undefined 
  f >>= m = undefined
  fail s = undefined


mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

plusPrimitive :: Primitive
plusPrimitive (IntVal n: IntVal m: []) = Right $ IntVal (n + m)
plusPrimitive (StringVal n: StringVal m: []) = Right $ StringVal (n ++ m)
plusPrimitive (IntVal n: StringVal m: []) = Right $ StringVal (show n ++ m)
plusPrimitive (StringVal n: IntVal m: []) = Right $ StringVal (n ++ show m)
plusPrimitive _ = Left "(+) called with wrong number or type of arguments"

minusPrimitive :: Primitive
minusPrimitive (IntVal n: IntVal m: []) = Right $ IntVal (n - m)
minusPrimitive _ = Left "(-) called with wrong number or type of arguments"

productPrimitive :: Primitive
productPrimitive (IntVal n: IntVal m: []) = Right $ IntVal (n * m)
productPrimitive _ = Left "(*) called with wrong number or type of arguments"

modPrimitive :: Primitive
modPrimitive (IntVal n: IntVal m: []) = Right $ IntVal (n `mod` m)
modPrimitive _ = Left "(%) called with wrong number or type of arguments"

lessThanPrimitive :: Primitive
lessThanPrimitive (IntVal n: IntVal m: []) = Right val 
 where val = if n < m then TrueVal else FalseVal
lessThanPrimitive (StringVal n: StringVal m: []) = Right val 
 where val = if n < m then TrueVal else FalseVal
lessThanPrimitive _ = Left "(<) called with wrong number or type of arguments"

equalityPrimitive :: Primitive
equalityPrimitive list
  | lengthList == 2 = Right val
  | otherwise = Left "(===) called with wrong number or type of arguments"
   where 
    lengthList = length list
    val = if (length list == 2) && (head list == last list) then TrueVal else FalseVal

-- modifyEnv :: (Env -> Env) -> SubsM ()
-- modifyEnv :: Env -> Maybe Value -> Ident -> Env
modifyEnv env = \f key-> Map.alter (\_ -> f) key env
-- (modifyEnv env $ removeValue) "x"
-- (modifyEnv env $ editValue(IntVal 4)) "x"

removeValue = Nothing
editValue = \x -> Just x 

-- putVar :: Ident -> Value -> SubsM ()
putVar name val ctx@(env, penv) = Map.insert name val env 

-- getVar :: Ident -> SubsM Value
getVar name ctx@(env, penv) = case Map.lookup name env of
                        Nothing -> Left "Var ident Error"
                        Just i -> Right i

-- getFunction :: FunName -> SubsM Primitive
getFunction :: FunName -> Context -> Either Error Primitive
getFunction name ctx@(env, penv) = case (Map.lookup name penv) of
                        Nothing -> Left "Call Error"
                        Just i -> Right i

eval :: Expr -> Context -> Either Error Value
eval (Number n) ctx = Right $ IntVal n
eval (String n) ctx = Right $ StringVal n
eval Undefined ctx = Right $ UndefinedVal
eval TrueConst ctx = Right $ TrueVal
eval FalseConst ctx = Right $ FalseVal

eval (Array []) ctx = Right $ ArrayVal []
eval (Array exprs) ctx = Right $ ArrayVal $ 
                   map (\expr -> getRightVal (eval expr ctx)) exprs

eval (Var ident) ctx = getVar ident ctx

eval (Call funName exprs) ctx@(env, penv) = case (getFunction funName ctx) of
                        Left error -> Left error
                        Right f -> f $ getArrayVal $ getRightVal $ eval (Array exprs) ctx

-- eval (Assign ident expr) ctx@(env, penv) = ((modifyEnv env $ removeValue) "x") >> eval expr ctx 

-- exec :: Expr -> Context -> Env
-- exec (Assign ident expr) ctx@(env, penv) = putVar ident (getRightVal $ eval expr ctx) ctx


getArrayVal :: Value -> [Value]                          
getArrayVal (ArrayVal x) = x 

getRightVal (Right i) = i
getLeftVal (Left i) = i

evalExpr :: Expr -> SubsM Value
evalExpr expr = undefined

runExpr :: Expr -> Either Error Value
runExpr expr = undefined

-- instance Num Expr where
--   Number x + Number y = Number (x+y)
--   Number x * Number y = Number (x*y)
--   negate (Number x) = Number (negate x)
--   abs (Number x) = Number (abs x)
--   signum (Number x) = Number (signum x)



