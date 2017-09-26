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

import SubsAst
import Parser.ParseFactor
import Parser.Utilities (whitespace)

import Text.Parsec hiding (Empty)
import Text.Parsec.Prim hiding (token, Empty)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Data.Char

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
