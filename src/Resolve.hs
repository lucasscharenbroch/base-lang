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
    , localOffset :: LocalOffset
    }

initialResolutionState :: ResolutionState
initialResolutionState = ResolutionState
    { symbolTable = []
    , tupleTable = empty
    , localOffset = 0
    }

type R = (Type, Location)
type SymbolTable = [Map String R]
type TupleTable = Map String [Decl]
type LocalOffset = Int -- number of words, starting at 0
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
addTupleDecl pos id_ decls = do
    tupTable <- tupleTable <$> get
    case lookup id_ tupTable of
        Just _ -> lift . Left $ "Multiply declared tuple type: `" ++ id_ ++ "` @" ++ show pos
        Nothing -> modify (\s -> s {tupleTable = insert id_ decls tupTable})

idLookup :: Id -> ResolveM (Maybe R)
idLookup id_ = do
    symTable <- symbolTable <$> get
    return $ case mapMaybe (lookup id_) symTable of
        [] -> Nothing
        x:_ -> Just x

tupleLookupOrErr :: SourcePos -> String -> ResolveM [Decl]
tupleLookupOrErr pos id_ = do
    tupTable <- tupleTable <$> get
    case lookup id_ tupTable of
        Just decls -> return decls
        Nothing -> lift . Left $ "Undefined tuple type: `" ++ id_ ++ "` @" ++ show pos

assertUniqueDecl :: Decl -> ResolveM ()
assertUniqueDecl (Decl pos _type id_) = do
    maybeMatch <- idLookup id_
    case maybeMatch of
        Just _ -> lift . Left $ "Multiply declared identifier: `" ++ id_ ++ "` @" ++ show pos
        Nothing -> return ()

addGlobalDecl :: Decl -> ResolveM ()
addGlobalDecl decl@(Decl _pos type_ id_) = do
    assertUniqueDecl decl
    let location = Label $ "_" ++ id_
    modifySymTable (\st -> insert id_ (type_, location) (head st) : tail st)

addLocalDecl :: Decl -> ResolveM ()
addLocalDecl decl@(Decl _pos type_ id_) = do
    assertUniqueDecl decl
    typeSize <- calcTypeSize _pos type_
    location <- allocateLocalSpace typeSize
    modifySymTable (\st -> insert id_ (type_, location) (head st) : tail st)

calcTypeSize :: SourcePos -> Type -> ResolveM Int
calcTypeSize pos (TValType vt) = case vt of
    VTInteger -> return 1
    VTLogical -> return 1
    VTString -> return 1 -- pointer
    VTTuple id_ -> do
        decls <- tupleLookupOrErr pos id_
        sum <$> mapM (\(Decl pos' type_ _id) -> calcTypeSize pos' type_) decls
calcTypeSize _ _ = error "Can't calculate size of non-value type"

allocateLocalSpace :: Int -> ResolveM Location
allocateLocalSpace n = do
    oldCnt <- localOffset <$> get
    modify (\s -> s {localOffset = oldCnt + n})
    return . Offset $ oldCnt

resolveTopDecl :: TopDecl () -> ResolveM (TopDecl R)
resolveTopDecl (FnDecl pos retType id_ params body) = undefined
resolveTopDecl (TupleDef pos id_ decls) = addTupleDecl pos id_ decls >> return (TupleDef pos id_ decls)
resolveTopDecl (Global decl) = addGlobalDecl decl >> return (Global decl)

resolveStmt :: Stmt () -> ResolveM (Stmt R)
resolveStmt = undefined

resolveExpr :: Expr () -> ResolveM (Expr R)
resolveExpr = undefined
