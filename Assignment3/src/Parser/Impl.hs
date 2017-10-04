module Parser.Impl (
    ParseError
  , parseString
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
import Text.Parsec.String

parseString :: String -> Either ParseError Expr
parseString = parse (do
                        res <- stripLeadingWhitespace parseExpr
                        eof
                        return res) "fail"

-- Parses an expression between parentheses
parentheses :: Parser Expr
parentheses = do
                 _ <- whitespace $ char '('
                 expr <- whitespace parseExpr
                 _ <- whitespace $ char ')'
                 return expr

-- Parses a factor as specified in the grammar
parseFactor :: Parser Expr
parseFactor = whitespace $
                  choice [ try parseArrayArrayFor
                         , parseArray
                         , try parseFunctionCall
                         , number
                         , parentheses
                         , try parseIdent
                         , stringParser
                         , parseTrue
                         , try parseFalse
                         , parseUndefined
                         
                         ]

-- Parses a Term in the grammar
parseTerm :: Parser Expr
parseTerm = do
               factor <- parseFactor
               parseTerm' factor

-- Parses a Term' in the grammar
parseTerm' :: Expr -> Parser Expr
parseTerm' input = (do
                       prodOp <- parseProdOp
                       factor <- parseFactor
                       parseTerm' $ Call [prodOp] [input, factor])
                   <|> return input

-- Parses a Comparable in the grammar
parseComparable :: Parser Expr
parseComparable = do
                     term <- parseTerm
                     parseComparable' term

-- Parses a Comparable' in the grammar
parseComparable' :: Expr -> Parser Expr
parseComparable' input = (do
                             addOp <- parseAddOp
                             term <- parseTerm
                             parseComparable' $ Call [addOp] [input, term])
                         <|> return input

-- Parses an Assignable in the grammar
parseAssignable :: Parser Expr
parseAssignable = do
                     comparable <- parseComparable
                     parseAssignable' comparable

-- Parses an Assignable' in the grammar
parseAssignable' :: Expr -> Parser Expr
parseAssignable' input = (do
                             compOp <- parseCompOp
                             comparable <- parseComparable
                             parseAssignable' $ Call compOp [input, comparable])
                         <|> return input

-- Parses an Assignment in the grammar
parseAssignment :: Parser Expr
parseAssignment = do
                     Var ident <- parseIdent
                     _ <- parseAssign
                     parseAssignment' ident

  -- Parses an Assignment' in the grammar
parseAssignment' :: Ident -> Parser Expr
parseAssignment' input = do
                            expr1 <- parseExpr1
                            return $ Assign input expr1

-- Parses an Expr in the grammar
parseExpr :: Parser Expr
parseExpr = do
               expr1 <- whitespace parseExpr1
               parseExpr' expr1

-- Parses an Expr' in the grammar
parseExpr' :: Expr -> Parser Expr
parseExpr' input = (do
                       _ <- parseComma
                       rest <- whitespace parseExpr
                       return $ Comma input rest)
                   <|> return input

-- Parses an Expr1 in the grammar
parseExpr1 :: Parser Expr
parseExpr1 = choice [ try parseAssignment
                    , parseAssignable
                    ]

-- Parses a function call (Ident (...) in the grammar)
parseFunctionCall :: Parser Expr
parseFunctionCall = do
                       Var ident <- parseIdent
                       _ <- char '('
                       args <- parseExprs
                       _ <- char ')'
                       return $ Call ident args

-- Parses an array
parseArray :: Parser Expr
parseArray = do
                _ <- whitespace $ char '['
                exprs <- whitespace parseExprs
                _ <- whitespace $ char ']'
                return $ Array exprs

-- Parses an ArrayFor within an array
parseArrayArrayFor :: Parser Expr
parseArrayArrayFor = do
                        _ <- whitespace $ char '['
                        compr <- parseArrayFor
                        _ <- whitespace $ char ']'
                        return $ Compr compr

-- Parses an ArrayFor
parseArrayFor :: Parser ArrayCompr
parseArrayFor = do
                   _ <- try $ whitespace $ parseKeyword "for"
                   _ <- whitespace $ char '('
                   Var ident <- whitespace parseIdent
                   _ <- whitespace $ parseKeyword "of"
                   expr1 <- whitespace parseExpr1
                   _ <- whitespace $ char ')'
                   compr <- parseArrayCompr
                   return $ ACFor ident expr1 compr

-- Parses an ArrayIf
parseArrayIf :: Parser ArrayCompr
parseArrayIf = do
                  _ <- whitespace $ parseKeyword "if"
                  _ <- whitespace $ char '('
                  expr1 <- whitespace parseExpr1
                  _ <- whitespace $ char ')'
                  compr <- parseArrayCompr
                  return $ ACIf expr1 compr

-- Parses any ArrayCompr
parseArrayCompr :: Parser ArrayCompr
parseArrayCompr = choice [ parseACBody
                         , parseArrayFor
                         , parseArrayIf
                         ]

-- Parses an ACBody
parseACBody :: Parser ArrayCompr
parseACBody = do
                 expr <- parseExpr1
                 return $ ACBody expr


-- Parses a list of expressions
parseExprs :: Parser [Expr]
parseExprs = do
                expr1 <- whitespace parseExpr1
                parseCommaExprs expr1
            <|> return []

-- Parses expressions separated by a comma
parseCommaExprs :: Expr -> Parser [Expr]
parseCommaExprs input = do
                        _ <- parseComma
                        rest <- whitespace parseExprs
                        return (input:rest)
                    <|> return [input]
