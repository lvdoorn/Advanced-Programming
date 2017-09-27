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

skipComment :: Parser ()
skipComment = do 
                 _ <- string "//"
                 _ <- manyTill anyChar (newLine <|> eof)
                 return ()

-- Wrapper for newline to be of type ()
newLine :: Parser ()
newLine = do
  _ <- newline
  return ()

-- Core imlementation of whitespace and comment skipping
whitespaceCore :: Parser ()
whitespaceCore = do spaces
                    optional skipComment
                    spaces

-- Removes whitespace after a string
whitespace :: Parser a -> Parser a
whitespace p = do res <- p
                  whitespaceCore
                  return res

-- Strips whitespace leading a string
stripLeadingWhitespace :: Parser a -> Parser a
stripLeadingWhitespace p = do whitespaceCore
                              p
