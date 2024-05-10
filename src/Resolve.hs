module Resolve where

import Prelude hiding (lookup)
import Data.Map (Map, insert, lookup, empty)
import Control.Monad.State.Lazy
import Text.Parsec (SourcePos)

import Ast
import Data.Maybe (mapMaybe)

data ResolutionState = ResolutionState
    { symbolTable :: SymbolTable
    , tupleTable :: TupleTable
    , localCount :: LocalCount
    }

initialResolutionState :: ResolutionState
initialResolutionState = ResolutionState
    { symbolTable = []
    , tupleTable = empty
    , localCount = 0
    }

type R = (ValueType, Location)
type SymbolTable = [Map String R]
type TupleTable = Map String [Decl]
type LocalCount = Int
type ResolveM = StateT ResolutionState (Either Error)

resolve :: UnresolvedAst -> Either Error ResolvedAst
resolve = flip evalStateT initialResolutionState . mapM resolveTopDecl

modifySymTable :: (SymbolTable -> SymbolTable) -> ResolveM ()
modifySymTable f = do
    symTable <- symbolTable <$> get
    modify (\s -> s {symbolTable = f symTable})

with :: ResolveM () -> ResolveM b -> ResolveM b
with change comp = do
    old <- get
    change
    comp <* put old

withNewScope :: ResolveM a -> ResolveM a
withNewScope = with $ modify (\s -> s {symbolTable = empty : symbolTable s})

addTupleDecl :: SourcePos -> String -> [Decl] -> ResolveM ()
addTupleDecl pos _id decls = do
    tupTable <- tupleTable <$> get
    case lookup _id tupTable of
        Just _ -> lift . Left $ "Multiply declared tuple type: `" ++ _id ++ "` @" ++ show pos
        Nothing -> modify (\s -> s {tupleTable = insert _id decls tupTable})

idLookup :: Id -> ResolveM (Maybe R)
idLookup _id = do
    symTable <- symbolTable <$> get
    return $ case mapMaybe (lookup _id) symTable of
        [] -> Nothing
        x:_ -> Just x

addGlobalDecl :: Decl -> ResolveM ()
addGlobalDecl = undefined

addLocalDecl :: Decl -> ResolveM ()
addLocalDecl = undefined

resolveTopDecl :: TopDecl () -> ResolveM (TopDecl R)
resolveTopDecl (FnDecl pos retType _id params body) = undefined
resolveTopDecl (TupleDef pos _id decls) = addTupleDecl pos _id decls >> return (TupleDef pos _id decls)
resolveTopDecl (Global decl) = addGlobalDecl decl >> return (Global decl)

resolveStmt :: Stmt () -> ResolveM (Stmt R)
resolveStmt = undefined

resolveExpr :: Expr () -> ResolveM (Expr R)
resolveExpr = undefined
