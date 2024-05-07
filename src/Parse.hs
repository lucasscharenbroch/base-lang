module Parse where

import Lex
import Error
import Ast

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
data Expr

parse :: String -> Either Error NoOffsetAst
parse = undefined
