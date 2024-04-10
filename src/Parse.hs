module Parse where

import Lex
import Error

type Ast = () -- TODO

parse :: [Token] -> Either Error Ast
parse = undefined
