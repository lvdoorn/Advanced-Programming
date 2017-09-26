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
  ) where

import SubsAst

import Text.Parsec hiding (Empty)
import Text.Parsec.Prim hiding (token, Empty)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Data.Char

parseString :: String -> Either ParseError Expr
parseString str = parse parseExpr "fail" str

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
                         , parseFalse
                         , parseUndefined
                         ]

parseTimes :: Parser Char
parseTimes = char '*'

parseMod :: Parser Char
parseMod = char '%'

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
parsePlus = char '+'

parseMinus :: Parser Char
parseMinus = char '-'

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
parseLessThan = string "<"

parseEquals :: Parser String
parseEquals = string "==="

parseCompOp :: Parser String
parseCompOp = parseLessThan <|> parseEquals

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
parseAssign = char '='

parseAssignment :: Parser Expr
parseAssignment = do
  ident <- parseIdent
  _ <- parseAssign
  parseAssignment' ident


-- TODO, need expr1 parser first
parseAssignment' :: Expr -> Parser Expr
parseAssignment' input = undefined

parseComma :: Parser Char
parseComma = char ','

parseExpr :: Parser Expr
parseExpr = do
  expr1 <- parseExpr1
  parseExpr' expr1

parseExpr' :: Expr -> Parser Expr
parseExpr' input = (do _ <- parseComma
                       rest <- parseExpr
                       return $ Comma input rest)
               <|> return input

parseExpr1 :: Parser Expr
parseExpr1 = choice [ parseAssignable
                    , parseAssignment
                    -- , parseFunctionCall
                    -- , parseArray
                    -- , parseArrayFor -- with Compr
                    ]

-- parseFunctionCall :: Parser Expr
-- parseFunctionCall = do ident <- parseIdent


parseFor :: Parser String
parseFor = string "for"

parseOf :: Parser String
parseOf = string "of"

-- parseArrayFor :: Parser ArrayCompr
-- parseArrayFor = do _ <- whitespace $ parseFor
--                    _ <- whitespace $ char '(' -- TODO replace with between combinator
--                    Var ident <- whitespace $ parseIdent
--                    _ <- whitespace $ parseOf
--                    expr1 <- whitespace $ parseExpr1
--                    _ <- whitespace $ char ')'
--                    compr <- parseArrayCompr
--                    return $ ACFor ident expr1 compr

-- parseIf :: Parser String
-- parseIf = string "if"

-- parseArrayIf :: Parser ArrayCompr
-- parseArrayIf = do _ <- whitespace $ parseIf
--                   _ <- whitespace $ char '('
--                   expr1 <- whitespace $ parseExpr1
--                   _ <- whitespace $ char ')'
--                   compr <- parseArrayCompr
--                   return $ ACIf expr1 compr

-- parseArrayCompr :: Parser Expr
-- parseArrayCompr = choice [ parseExpr1
--                          , parseArrayFor
--                          , parseArrayIf
--                          ]




-- TODO combine with array parsing
-- parseExprs :: Parser Expr
-- parseExprs = do
--   expr1 <- parseExpr1
--   parseCommaExprs expr1

-- parseCommaExprs :: Expr -> Parser Expr
-- parseCommaExprs input = do _ <- whitespace $ parseComma
--                            expr1 <- whitespace $ parseExpr1
--                            parseCommaExprs $ expr1