module SubsInterpreter
       (
         Value(..)
       , SubsM(..)
       , runExpr
       , evalExpr
       , initialContext
       , getFunction
       , plusPrimitive
       , env
       , context
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

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  fmap f m = m >>= \a -> return(f a)

instance Applicative SubsM where
  pure = return;
  (<*>) = ap

-- m a -> (a -> m b) -> m b
instance Monad SubsM where
  return x = SubsM (\(e,p) -> Right (x, e))
  m >>= f = SubsM $ \(e,p) -> let res = runSubsM m (e,p) in
    case res of
      Left err -> Left err
      Right (val, e') -> runSubsM (f val) (e',p)
  fail s = SubsM (\x -> Left s)




--type Primitive = [Value] -> Either Error Value

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM (\(e,p) -> Right ((), f e))

putVar :: Ident -> Value -> SubsM ()
putVar name val = SubsM (\(e,p) -> Right ((), Map.insert name val e))

putVar2 :: Ident -> Value -> SubsM Value
putVar2 name val = SubsM (\(e,p) -> Right (val, Map.insert name val e))

getVar :: Ident -> SubsM Value
getVar name = SubsM (\(e,p) -> case Map.lookup name e of
                        Nothing -> Left "Var not found"
                        Just i ->  Right (i, e)
                    )

getFunction :: FunName -> SubsM Primitive
getFunction name = case (Map.lookup name $ snd initialContext) of
                        Nothing -> fail "Call Error: function does not exist"
                        Just i -> return i

evalExpr :: Expr -> SubsM Value
evalExpr (Number i) = return $ IntVal i
evalExpr (String s) = return $ StringVal s
evalExpr Undefined = return $ UndefinedVal
evalExpr TrueConst = return $ TrueVal
evalExpr FalseConst = return $ FalseVal
evalExpr (Array []) = return $ ArrayVal []
evalExpr (Array exprs) = helper exprs
evalExpr (Var s) = getVar s
evalExpr (Call func exprs) = do x <- getFunction func
                                y <- helper2 exprs >>= applyPrimitive x
                                return y
evalExpr (Assign s expr) = evalExpr expr >>= putVar2 s
evalExpr (Comma expr1 expr2) = evalExpr expr1 >> evalExpr expr2

helper :: [Expr] -> SubsM Value
helper [] = return $ ArrayVal []
helper (e:xs) = do res <- evalExpr e
                   ArrayVal res2 <- helper xs
                   return $ ArrayVal (res:res2)

helper2 :: [Expr] -> SubsM [Value]
helper2 [] = return []
helper2 (e:xs) = do res <- evalExpr e
                    res2 <- helper2 xs
                    return (res:res2)

applyPrimitive :: Primitive -> [Value] -> SubsM Value
applyPrimitive pr list = case pr list of
  Left err -> fail err
  Right res -> return res

-- eval (Var ident) ctx = getVar ident ctx

-- eval (Call funName exprs) ctx@(env, penv) = case (getFunction funName ctx) of
--                         Left error -> Left error
                        -- Right f -> f $ getArrayVal $ getRightVal $ eval (Array exprs) ctx

runExpr :: Expr -> Either Error Value
runExpr expr = case (runSubsM $ evalExpr expr) (initialContext) of
  Left err -> Left err
  Right (val, env) -> Right val






env :: Map Ident Value
env = Map.fromList [("x", IntVal 1), ("y", StringVal "myString")]

penv :: Map FunName Primitive
penv = Map.fromList [("*", mkArray), ("+", mkArray)]

context = (env, penv)