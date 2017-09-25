module Parser.Impl (
  parseString,
  ParseError,
  posNumber,
  negNumber,
  number,
  getBackslashChar,
  parseNewline,
  isValid,
  parseChar,
  backslash,
  stringParser,
  parse,
  whitespace

  ) where

import SubsAst

import Text.Parsec
import Text.Parsec.Prim hiding (token)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Data.Char

parseString :: String -> Either ParseError Expr
parseString = undefined

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
stringParser = do
  _ <- (char '\'')
  str <- many parseChar
  _ <- (char '\'')
  return $ String str