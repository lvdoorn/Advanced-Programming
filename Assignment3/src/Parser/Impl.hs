module Parser.Impl (
  parseString,
  ParseError,
  number,
  posNumber,
  negNumber,
  backslash,
  stringParser,
  parse

  ) where

import SubsAst
-- import Text.ParserCombinators.Parsec hiding (ParseError)

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Data.Char

parseString :: String -> Either ParseError Expr
parseString = undefined

posNumber :: Parser Expr
posNumber = do
    n <- many1 digit
    return $ Number $ (read n)

negNumber :: Parser Expr
negNumber = do
  _ <- (char '-')
  n <- many1 digit
  return $ Number $ ((read n) * (-1))

-- TODO: Eight digit limit on numbers
number :: Parser Expr
number = negNumber <|> posNumber

-- TODO: newlines
getBackslashChar :: Char -> Either ParseError Char
getBackslashChar c | c == 'n' = Right '\n'
                   | c == 't' = Right '\t'
                   | c `elem` ['\'', '\\'] = Right c
                   | otherwise = fail "Backslash followed by invalid letter"

parseNewline :: Parser ()
parseNewline = do
  a <- (char '\\')
  b <- (char '\n') -- can be replaced by newline
  return ()

backslash :: Parser Char
backslash = do
  _ <- (char '\\')
  c <- oneOf ['\'', 'n', 't', '\\', '\n']
  case getBackslashChar c of
    Right char -> return char
    --Left error -> fail error -- TODO(?)

isValid :: Char -> Bool
isValid c | c == '\'' = False
          | c == '\\' = False
          | ord c >= 32 && ord c <= 126 = True
          | otherwise = False



parseChar :: Parser Char
parseChar = do 
  _ <- optional parseNewline
  backslash <|> (satisfy isValid)

stringParser :: Parser Expr
stringParser = do
  _ <- (char '\'')
  str <- many parseChar
  _ <- (char '\'')
  return $ String str