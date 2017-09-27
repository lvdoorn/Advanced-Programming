module Parser.Utilities (
    parseKeyword
  , parseComma
  , whitespace
  , stripLeadingWhitespace
) where

import Text.Parsec hiding (Empty)
import Text.Parsec.String


parseKeyword :: String -> Parser ()
parseKeyword str = whitespace $ do string str
                                   notFollowedBy alphaNum

parseFor :: Parser String
parseFor = whitespace $ string "for"

parseOf :: Parser String
parseOf = whitespace $ string "of"

parseComma :: Parser Char
parseComma = whitespace $ char ','

-- skipComment :: Parser a -> Parser a
-- skipComment p = do res <- p
--                    string "//"
--                    manyTill anyChar (newline <|> eof)
--                    return res

-- Copied from slide 14 of second parser lecture
whitespace :: Parser a -> Parser a
whitespace p = do res <- p
                  spaces
                  optional $ (do string "//"
                                 (manyTill anyChar eof) <|> (manyTill anyChar newline)

                              )
                  -- _ <- try $ optional (do string "//"
                  --                         manyTill anyChar newline)
                  -- _ <- try $ optional (do string "//"
                  --                         manyTill anyChar eof)
                  return res
stripLeadingWhitespace :: Parser a -> Parser a
stripLeadingWhitespace p = do spaces
                              optional $ (do string "//"
                                             (try (manyTill anyChar eof)) <|> (manyTill anyChar newline)

                                          )
                              p