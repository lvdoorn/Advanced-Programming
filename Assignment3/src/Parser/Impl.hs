module Parser.Impl (
  parseString,
  ParseError



  ) where

import SubsAst
import Text.ParserCombinators.Parsec hiding (ParseError)

-- can change this if you want, but must be an instance of Show and Eq
data ParseError = ParseError String
                deriving (Show, Eq)

parseString :: String -> Either ParseError Expr
parseString = undefined

number :: Parser Integer
number = do
    n <- many1 digit
    return (read n)