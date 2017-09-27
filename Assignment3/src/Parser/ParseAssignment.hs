module Parser.ParseAssignment (
    parseTimes
  , parseMod
  , parseProdOp
  , parsePlus
  , parseMinus
  , parseAddOp
  , parseLessThan
  , parseEquals
  , parseCompOp
  , parseAssign
) where

import Parser.Utilities (whitespace)

import Text.Parsec hiding (Empty)
import Text.Parsec.String

parseTimes :: Parser Char
parseTimes = whitespace $ char '*'

parseMod :: Parser Char
parseMod = whitespace $ char '%'

parseProdOp :: Parser Char
parseProdOp = parseTimes <|> parseMod

parsePlus :: Parser Char
parsePlus = whitespace $ char '+'

parseMinus :: Parser Char
parseMinus = whitespace $ char '-'

parseAddOp :: Parser Char
parseAddOp = parsePlus <|> parseMinus

parseLessThan :: Parser String
parseLessThan = whitespace $ string "<"

parseEquals :: Parser String
parseEquals = whitespace $ string "==="

parseCompOp :: Parser String
parseCompOp = parseLessThan <|> try parseEquals

parseAssign :: Parser Char
parseAssign = whitespace $ char '='
