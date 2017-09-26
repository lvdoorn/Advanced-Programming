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
  , factorParser
  , parseIdent
  , parseTerm
  , parseComparable
  , parseAssignable
  , parseAssignment
  ) where

import SubsAst

import Text.Parsec hiding (Empty)
import Text.Parsec.Prim hiding (token, Empty)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Data.Char

parseString :: String -> Either ParseError Expr
parseString str = parse (stripLeadingWhitespace parseExpr) "fail" str

stripLeadingWhitespace :: Parser a -> Parser a
stripLeadingWhitespace p = do spaces
                              res <- p
                              return res;

-- Copied from slide 14 of second parser lecture
whitespace :: Parser a -> Parser a
whitespace p = do res <- p
                  spaces
                  return res


-- Parses a positive number
posNumber :: Parser Expr
posNumber = do
    n <- many1 digit
    return $ Number $ (read n)

-- Parses a negative number
negNumber :: Parser Expr
negNumber = do
  _ <- (char '-')
  n <- many1 digit
  return $ Number $ ((read n) * (-1))

-- TODO: Eight digit limit on numbers
-- Parses any number
number :: Parser Expr
number = whitespace $ negNumber <|> posNumber

-- TODO: newlines

-- Returns the correct char matching a backslash sequence
getBackslashChar :: Char -> Either ParseError Char
getBackslashChar c | c == 'n' = Right '\n'
                   | c == 't' = Right '\t'
                   | c `elem` ['\'', '\\'] = Right c
                   | otherwise = fail "Backslash followed by invalid letter"

-- Parses a newline character after a backslash used to run a string over multiple lines
parseNewline :: Parser ()
parseNewline = do
  a <- (char '\\')
  b <- (char '\n') -- can be replaced by newline (parser)
  return ()

-- Parses the backslash characters from SubScript
backslash :: Parser Char
backslash = do
  _ <- (char '\\')
  c <- oneOf ['\'', 'n', 't', '\\', '\n']
  case getBackslashChar c of
    Right char -> return char
    --Left error -> fail error -- TODO(?)

-- Checks if a character is allowed in a SubScript string
isValid :: Char -> Bool
isValid c | c == '\'' = False
          | c == '\\' = False
          | ord c >= 32 && ord c <= 126 = True
          | otherwise = False


-- Parses a single character in a string constant
parseChar :: Parser Char
parseChar = do 
  _ <- optional parseNewline
  backslash <|> (satisfy isValid)

-- Parses a string constant
stringParser :: Parser Expr
stringParser = whitespace $ do
  _ <- (char '\'')
  str <- many parseChar
  _ <- (char '\'')
  return $ String str

-- Parses the `true' keyword
parseTrue :: Parser Expr
parseTrue = whitespace $ do
  string "true"
  return TrueConst

-- Parses the `false' keyword
parseFalse :: Parser Expr
parseFalse = whitespace $ do
  string "false"
  return FalseConst

-- Parses the `undefined' keyword
parseUndefined :: Parser Expr
parseUndefined = whitespace $ do
  string "undefined"
  return Undefined

-- TODO
parentheses :: Parser Expr
parentheses = do _ <- whitespace $ char '('
                 expr <- whitespace $ parseExpr
                 _ <- whitespace $ char ')'
                 return expr


keywords :: [String]
keywords = ["true", "false", "undefined", "for", "of", "if"]

-- Parses an identifier
parseIdent :: Parser Expr
parseIdent = whitespace $ do
    fc <- firstChar
    rest <- many nonFirstChar
    let inputId = fc:rest 
    if inputId `notElem` keywords then return (Var inputId) 
                                  else fail "should not be a keyword"
  where
    firstChar = letter
    nonFirstChar = digit <|> firstChar <|> char '_'

-- TODO: Uniform naming conventions for parsers
-- Parses a factor as specified in the grammar
factorParser :: Parser Expr
factorParser = whitespace $
                  choice [ number
                         , parentheses
                         , try parseIdent
                         , stringParser
                         , parseTrue
                         , try parseFalse
                         , parseUndefined
                         ]

parseTimes :: Parser Char
parseTimes = whitespace $ char '*'

parseMod :: Parser Char
parseMod = whitespace $ char '%'

parseProdOp :: Parser Char
parseProdOp = parseTimes <|> parseMod

parseTerm :: Parser Expr
parseTerm = do
  factor <- factorParser
  parseTerm' factor

parseTerm' :: Expr -> Parser Expr
parseTerm' input = (do prodOp <- parseProdOp
                       factor <- factorParser
                       parseTerm' $ Call [prodOp] [input, factor])
               <|> return input

parsePlus :: Parser Char
parsePlus = whitespace $ char '+'

parseMinus :: Parser Char
parseMinus = whitespace $ char '-'

parseAddOp :: Parser Char
parseAddOp = parsePlus <|> parseMinus

parseComparable :: Parser Expr
parseComparable = do
  term <- parseTerm
  parseComparable' term

parseComparable' :: Expr -> Parser Expr
parseComparable' input = (do addOp <- parseAddOp
                             term <- parseTerm
                             parseComparable' $ Call [addOp] [input, term])
                     <|> return input

parseLessThan :: Parser String
parseLessThan = whitespace $ string "<"

parseEquals :: Parser String
parseEquals = whitespace $ string "==="

parseCompOp :: Parser String
parseCompOp = parseLessThan <|> try parseEquals

parseAssignable :: Parser Expr
parseAssignable = do
  comparable <- parseComparable
  parseAssignable' comparable

parseAssignable' :: Expr -> Parser Expr
parseAssignable' input = (do compOp <- parseCompOp
                             assignable <- parseAssignable
                             parseAssignable' $ Call compOp [input, assignable])
                     <|> return input

parseAssign :: Parser Char
parseAssign = whitespace $ char '='

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

parseComma :: Parser Char
parseComma = whitespace $ char ','

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

parseFor :: Parser String
parseFor = whitespace $ string "for"

parseOf :: Parser String
parseOf = whitespace $ string "of"

parseArrayFor :: Parser ArrayCompr
parseArrayFor = do _ <- try $ whitespace $ parseFor
                   _ <- whitespace $ char '(' -- TODO replace with between combinator
                   Var ident <- whitespace $ parseIdent
                   _ <- whitespace $ parseOf
                   expr1 <- whitespace $ parseExpr1
                   _ <- whitespace $ char ')'
                   compr <- parseArrayCompr
                   return $ ACFor ident expr1 compr

parseIf :: Parser String
parseIf = whitespace $ string "if"

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