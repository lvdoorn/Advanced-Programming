module Parser.Impl where

import SubsAst

-- can change this if you want, but must be an instance of Show and Eq
data ParseError = ParseError String
                deriving (Show, Eq)

parseString :: String -> Either ParseError Expr
parseString = undefined

