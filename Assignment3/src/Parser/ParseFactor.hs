module Parser.ParseFactor (
    posNumber
  , negNumber
  , number
  , getBackslashChar
  , parseNewline
  , backslash
  , isValid
  , parseChar
  , stringParser
  , parseTrue
  , parseFalse
  , parseUndefined
  , keywords
  , parseIdent

) where

import SubsAst
import Parser.Utilities (whitespace)

import Text.Parsec hiding (Empty)
import Text.Parsec.Prim hiding (token, Empty)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Data.Char

import Debug.Trace

-- Parses a positive number
posNumber :: Parser Expr
posNumber = do
  n <- many1 digit
  if length (show n) <= 10 then return $ Number $ read n else fail "Int too long"

-- Parses a negative number
negNumber :: Parser Expr
negNumber = do
  _ <- char '-'
  n <- many1 digit
  if length (show n) <= 10 then return $ Number (read n * (-1)) else fail "Int too long"

-- TODO: Eight digit limit on numbers
-- Parses any number
number :: Parser Expr
number = do res <- whitespace $ negNumber <|> posNumber
            notFollowedBy $ char '.'
            return res
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
  a <- char '\\'
  b <- char '\n' -- can be replaced by newline (parser)
  return ()

-- Parses the backslash characters from SubScript
backslash :: Parser Char
backslash = do
  _ <- char '\\'
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
  backslash <|> satisfy isValid

-- Parses a string constant
stringParser :: Parser Expr
stringParser = whitespace $ do
  _ <- char '\''
  str <- many parseChar
  _ <- char '\''
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
