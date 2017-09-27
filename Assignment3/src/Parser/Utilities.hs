module Parser.Utilities (
    parseKeyword
  , parseComma
  , whitespace
  , stripLeadingWhitespace
) where

import Text.Parsec hiding (Empty)
import Text.Parsec.String


parseKeyword :: String -> Parser ()
parseKeyword str = whitespace $ do 
                                   _ <- string str
                                   notFollowedBy alphaNum

parseComma :: Parser Char
parseComma = whitespace $ char ','

-- Copied from slide 14 of second parser lecture
whitespace :: Parser a -> Parser a
whitespace p = do res <- p
                  spaces
                  return res

stripLeadingWhitespace :: Parser a -> Parser a
stripLeadingWhitespace p = do spaces
                              p