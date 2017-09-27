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

-- Skips a comment
skipComment :: Parser ()
skipComment = do string "//"
                 _ <- manyTill anyChar (newLine <|> eof)
                 return ()

-- Wrapper for newline to be of type ()
newLine :: Parser ()
newLine = do
  _ <- newline
  return ()

-- Copied from slide 14 of second parser lecture
whitespace :: Parser a -> Parser a
whitespace p = do res <- p
                  spaces
                  optional skipComment
                  spaces
                  return res

-- Strips whitespace leading a string
stripLeadingWhitespace :: Parser a -> Parser a
stripLeadingWhitespace p = do
  spaces
  optional skipComment
  spaces
  p