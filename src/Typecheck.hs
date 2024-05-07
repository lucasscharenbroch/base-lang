module Typecheck where

import Parse
import Error
import Ast

type TypedAst = () -- TODO

{-
data Type = TVoid
          |
-}

typecheck :: OffsetAst -> Either Error ()
typecheck = undefined
