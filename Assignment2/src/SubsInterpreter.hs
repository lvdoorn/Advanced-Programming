module SubsInterpreter
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

putVar :: Ident -> Value -> SubsM ()
putVar name val = SubsM (\(e,_) -> Right ((), Map.insert name val e))

getVar :: Ident -> SubsM Value
getVar name = SubsM (\(e,_) -> case Map.lookup name e of
                        Nothing -> Left "Var not found"
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
evalExpr (Call f exprs) = do primitiveFunc <- getFunction f
                             arrayValExprs <- evalExpr (Array exprs)
                             applyPrimitive primitiveFunc arrayValExprs
evalExpr (Assign ident expr) = do exprVal <- evalExpr expr
                                  _ <- putVar ident exprVal
                                  return exprVal
evalExpr (Comma expr1 expr2) = evalExpr expr1 >> evalExpr expr2
evalExpr (Compr compr) = evalCompr compr

evalCompr :: ArrayCompr -> SubsM Value
evalCompr (ACBody expr) = evalExpr expr
evalCompr (ACFor identifier expr compr) = do arr <- evalExpr expr
                                             func identifier arr compr

-- Assume if only occurs after at least one for
evalCompr (ACIf expr compr) = do res <- evalExpr expr
                                 case res of
                                    TrueVal -> evalCompr compr
                                    FalseVal -> return $ ArrayVal []
                                    _ -> fail "Not a boolean"

func :: Ident -> Value -> ArrayCompr -> SubsM Value
func _ (ArrayVal []) _ = return $ ArrayVal []
func i (ArrayVal arr) compr = do
                      _ <- putVar i (head arr)
                      case compr of
                        ACIf expr c -> do res <- evalIf $ ACIf expr c
                                          ArrayVal list <- func i (ArrayVal (tail arr)) compr
                                          return $ ArrayVal $ res ++ list
                        _ -> do res <- evalCompr compr
                                ArrayVal list <- func i (ArrayVal (tail arr)) compr
                                return $ ArrayVal $ res:list
func _ _ _ = fail "func called with wrong parameters"

evalIf :: ArrayCompr -> SubsM [Value]
evalIf (ACIf expr compr) = do res <- evalExpr expr
                              ret <- evalCompr compr
                              case res of
                                TrueVal -> return [ret]
                                FalseVal -> return []
                                _ -> fail "Error - if yielded other value than bool"
evalIf _ = fail "evalIf compr is not an ACIf"

helper :: [Expr] -> SubsM Value
helper [] = return $ ArrayVal []
helper (e:xs) = do res <- evalExpr e
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
  Right (val, _) -> Right val