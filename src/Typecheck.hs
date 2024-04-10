module Typecheck where

import Parse
import Error

type TypedAst = () -- TODO

typecheck :: Ast -> Either Error TypedAst
typecheck = undefined
