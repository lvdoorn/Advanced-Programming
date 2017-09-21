module SubsParser (
    ParseError,
    parseString,
    parseFile
  ) where

import SubsAst
import Parser.Impl

-- shouldn't need to change this
parseFile :: FilePath -> IO (Either ParseError Expr)
parseFile path = fmap parseString $ readFile path
