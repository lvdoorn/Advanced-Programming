module Parser.Impl (
    parseString
  , ParseError
  , posNumber
  , negNumber
  , number
  , getBackslashChar
  , parseNewline
  , isValid
  , parseChar
  , backslash
  , stringParser
  , parse
  , whitespace
  , parseTrue
  , parseFalse
  , parseUndefined
  , parseFactor
  , parseIdent
  , parseTerm
  , parseComparable
  , parseAssignable
  , parseAssignment
  ) where

import SubsAst
import Parser.Utilities
import Parser.ParseFactor
import Parser.ParseAssignment

import Text.Parsec hiding (Empty)
import Text.Parsec.Prim hiding (token, Empty)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator


import Data.Char

import Debug.Trace -- TODO: delete before handing in

parseString :: String -> Either ParseError Expr
parseString str = parse (stripLeadingWhitespace parseExpr) "fail" str





parentheses :: Parser Expr
parentheses = do _ <- whitespace $ char '('
                 expr <- whitespace $ parseExpr
                 _ <- whitespace $ char ')'
                 return expr

-- TODO: Uniform naming conventions for parsers
-- Parses a factor as specified in the grammar
parseFactor :: Parser Expr
parseFactor = whitespace $
                  choice [ number
                         , parentheses
                         , try parseIdent
                         , stringParser
                         , parseTrue
                         , try parseFalse
                         , parseUndefined
                         ]

parseTerm :: Parser Expr
parseTerm = do
  factor <- parseFactor
  parseTerm' factor

parseTerm' :: Expr -> Parser Expr
parseTerm' input = (do prodOp <- parseProdOp
                       factor <- parseFactor
                       parseTerm' $ Call [prodOp] [input, factor])
               <|> return input

parseComparable :: Parser Expr
parseComparable = do
  term <- parseTerm
  parseComparable' term

parseComparable' :: Expr -> Parser Expr
parseComparable' input = (do addOp <- parseAddOp
                             term <- parseTerm
                             parseComparable' $ Call [addOp] [input, term])
                     <|> return input

parseAssignable :: Parser Expr
parseAssignable = do
  comparable <- parseComparable
  parseAssignable' comparable

parseAssignable' :: Expr -> Parser Expr
parseAssignable' input = (do compOp <- parseCompOp
                             comparable <- parseComparable
                             parseAssignable' $ Call compOp [input, comparable])
                     <|> return input

parseAssignment :: Parser Expr
parseAssignment = do
  Var ident <- parseIdent
  _ <- parseAssign
  parseAssignment' ident

parseAssignment' :: Ident -> Parser Expr
parseAssignment' input = (do assignment <- parseAssignment
                             return $ Assign input assignment)
                     <|> (do expr1 <- parseExpr1
                             return $ Assign input expr1)



parseExpr :: Parser Expr
parseExpr = do
  expr1 <- whitespace $ parseExpr1
  parseExpr' expr1

parseExpr' :: Expr -> Parser Expr
parseExpr' input = (do _ <- parseComma
                       rest <- whitespace $ parseExpr
                       return $ Comma input rest)
               <|> return input

parseExpr1 :: Parser Expr
parseExpr1 = choice [ try parseArrayArrayFor
                    , parseArray
                    , try parseFunctionCall
                    , try parseAssignment
                    , parseAssignable
                    ]

parseFunctionCall :: Parser Expr
parseFunctionCall = do Var ident <- parseIdent
                       _ <- char '('
                       args <- parseExprs
                       _ <- char ')'
                       return $ Call ident args

parseArray :: Parser Expr
parseArray = do _ <- whitespace $ char '['
                exprs <- whitespace $ parseExprs
                _ <- whitespace $ char ']'
                return $ Array exprs

parseArrayArrayFor :: Parser Expr
parseArrayArrayFor = do _ <- whitespace $ char '['
                        compr <- parseArrayFor
                        _ <- whitespace $ char ']'
                        return $ Compr compr



parseArrayFor :: Parser ArrayCompr
parseArrayFor = do _ <- try $ whitespace $ parseFor
                   _ <- whitespace $ char '(' -- TODO replace with between combinator
                   Var ident <- whitespace $ parseIdent
                   _ <- whitespace $ parseOf
                   expr1 <- whitespace $ parseExpr1
                   _ <- whitespace $ char ')'
                   compr <- parseArrayCompr
                   return $ ACFor ident expr1 compr



parseArrayIf :: Parser ArrayCompr
parseArrayIf = do _ <- whitespace $ parseIf
                  _ <- whitespace $ char '('
                  expr1 <- whitespace $ parseExpr1
                  _ <- whitespace $ char ')'
                  compr <- parseArrayCompr
                  return $ ACIf expr1 compr

parseArrayCompr :: Parser ArrayCompr
parseArrayCompr = choice [ parseACBody
                         , parseArrayFor
                         , parseArrayIf
                         ]

parseACBody :: Parser ArrayCompr
parseACBody = do expr <- parseExpr1
                 return $ ACBody expr


-- TODO combine with array parsing
parseExprs :: Parser [Expr]
parseExprs = do expr1 <- whitespace $ parseExpr1
                parseCommaExprs expr1
         <|> return []

parseCommaExprs :: Expr -> Parser [Expr]
parseCommaExprs input = do _ <- parseComma
                           tail <- whitespace $ parseExprs
                           return (input:tail)
                    <|> return [input]





-- parseExpr :: Parser Expr
-- parseExpr = do
--   expr1 <- whitespace $ parseExpr1
--   parseExpr' expr1

-- parseExpr' :: Expr -> Parser Expr
-- parseExpr' input = (do _ <- parseComma
--                        rest <- whitespace $ parseExpr
--                        return $ Comma input rest)
--                <|> return input