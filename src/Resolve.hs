module Resolve where

import Data.Map
import Control.Monad.State.Lazy

import Ast

type R = (ValueType, Location)
type SymbolTable = [Map String R]
type ResolveM = StateT SymbolTable (Either Error)

resolve :: UnresolvedAst -> Either Error ResolvedAst
resolve = flip evalStateT [] . mapM resolveTopDecl

resolveTopDecl :: TopDecl () -> ResolveM (TopDecl R)
resolveTopDecl = undefined

resolveStmt :: Stmt () -> ResolveM (Stmt R)
resolveStmt = undefined

resolveExpr :: Expr () -> ResolveM (Expr R)
resolveExpr = undefined
