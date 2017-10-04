module Interpreter.Impl
       (
         Value(..)
       , SubsM(..)
       , runExpr
       , evalExpr
       , initialContext
       , getFunction
       , plusPrimitive
       , getVar
       , putVar
       , modifyEnv
       , applyPrimitive
       )
       where

import SubsAst

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
           | DummyVal -- Dummy value to show that getVar fails
                      -- without throwing an error
           deriving (Eq, Show)

type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> Either Error Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

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

plusPrimitive :: Primitive
plusPrimitive [IntVal n, IntVal m] = Right $ IntVal (n + m)
plusPrimitive [StringVal n, StringVal m] = Right $ StringVal (n ++ m)
plusPrimitive [IntVal n, StringVal m] = Right $ StringVal (show n ++ m)
plusPrimitive [StringVal n, IntVal m] = Right $ StringVal (n ++ show m)
plusPrimitive _ = Left "(+) called with wrong number or type of arguments"

minusPrimitive :: Primitive
minusPrimitive [IntVal n, IntVal m] = Right $ IntVal (n - m)
minusPrimitive _ = Left "(-) called with wrong number or type of arguments"

productPrimitive :: Primitive
productPrimitive [IntVal n, IntVal m] = Right $ IntVal (n * m)
productPrimitive _ = Left "(*) called with wrong number or type of arguments"

modPrimitive :: Primitive
modPrimitive [IntVal _, IntVal 0] = Left "Divide by zero"
modPrimitive [IntVal n, IntVal m] = Right $ IntVal (n `mod` m)
modPrimitive _ = Left "(%) called with wrong number or type of arguments"

lessThanPrimitive :: Primitive
lessThanPrimitive [IntVal n, IntVal m] = Right val
 where val = if n < m then TrueVal else FalseVal
lessThanPrimitive [StringVal n, StringVal m] = Right val
 where val = if n < m then TrueVal else FalseVal
lessThanPrimitive _ = Left "(<) called with wrong number or type of arguments"

equalityPrimitive :: Primitive
equalityPrimitive [x, y] = if x == y then Right TrueVal else Right FalseVal
equalityPrimitive _ = Left "(===) called with wrong number or type of arguments"

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  fmap f m = m >>= \a -> return(f a)

instance Applicative SubsM where
  pure = return
  (<*>) = ap

instance Monad SubsM where
  return x = SubsM (\(e,_) -> Right (x, e))
  m >>= f = SubsM $ \(e,p) -> let res = runSubsM m (e,p) in
    case res of
      Left err -> Left err
      Right (val, e') -> runSubsM (f val) (e',p)
  fail s = SubsM (\_ -> Left s)

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM (\(e,_) -> Right ((), f e))

restoreEnv :: Value -> Ident -> Env -> Env -> Env
restoreEnv oldval ident oldenv newenv = if Map.member ident oldenv
                                          then Map.insert ident oldval newenv
                                          else Map.delete ident newenv

getEnv :: SubsM Env
getEnv = SubsM $ \(env,_) -> (Right (env,env))

putVar :: Ident -> Value -> SubsM ()
putVar name val = SubsM (\(e,_) -> Right ((), Map.insert name val e))

getVar :: Ident -> SubsM Value
getVar name = SubsM (\(e,_) -> case Map.lookup name e of
                        Nothing -> Right (DummyVal, e)
                        Just i ->  Right (i, e)
                    )

getFunction :: FunName -> SubsM Primitive
getFunction name = case Map.lookup name $ snd initialContext of
                        Nothing -> fail "Call Error: function does not exist"
                        Just i -> return i

evalExpr :: Expr -> SubsM Value
evalExpr (Number i) = return $ IntVal i
evalExpr (String s) = return $ StringVal s
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Array []) = return $ ArrayVal []
evalExpr (Array exprs) = helper exprs
evalExpr (Var s) = getVar s
evalExpr (Call f exprs) = do
  primitiveFunc <- getFunction f
  arrayValExprs <- evalExpr (Array exprs)
  applyPrimitive primitiveFunc arrayValExprs
evalExpr (Assign ident expr) = do
  exprVal <- evalExpr expr
  _ <- putVar ident exprVal
  return exprVal
evalExpr (Comma expr1 expr2) = evalExpr expr1 >> evalExpr expr2
evalExpr (Compr compr) = do
  res <- evalCompr compr
  return $ ArrayVal res

evalCompr :: ArrayCompr -> SubsM [Value]
evalCompr (ACBody expr) = do
  res <- evalExpr expr
  return [res]
evalCompr (ACIf expr compr) = do
  res <- evalExpr expr
  case res of
    TrueVal  -> evalCompr compr
    FalseVal -> return []
    _ -> fail "Not a boolean"
evalCompr (ACFor ident expr compr) = do
  oldenv <- getEnv
  oldval <- getVar ident
  arr <- evalExpr expr
  case arr of
    ArrayVal array -> do
      res <- func ident array compr
      modifyEnv (restoreEnv oldval ident oldenv)
      return res
    _ -> fail "Not an array"


func :: Ident -> [Value] -> ArrayCompr -> SubsM [Value]
func _ [] _ = return []
func ident (x:xs) compr = do
  _ <- putVar ident x
  xres <- evalCompr compr
  xsres <- func ident xs compr
  return $ xres ++ xsres

helper :: [Expr] -> SubsM Value
helper [] = return $ ArrayVal []
helper (e:xs) = do
  res <- evalExpr e
  ArrayVal res2 <- helper xs
  return $ ArrayVal (res:res2)

applyPrimitive :: Primitive -> Value -> SubsM Value
applyPrimitive pr val = case val of
 (ArrayVal list) -> case pr list of
                    Left err -> fail err
                    Right res -> return res
 _ -> fail "Error applyPrimitive expects an ArrayVal"

runExpr :: Expr -> Either Error Value
runExpr expr = case (runSubsM $ evalExpr expr) initialContext of
  Left err -> Left err
  -- Catch the dummy values given by getVar
  Right (DummyVal, _) -> Left "Var not found"
  Right (val, _) -> Right val
