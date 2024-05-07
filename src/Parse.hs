module Parse where

import Error
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

type Ast = () -- TODO

parse :: String -> Either Error Ast
parse = undefined
