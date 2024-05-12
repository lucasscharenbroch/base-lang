module Resolve where

import Data.Map (Map, insert, empty)
import qualified Data.Map as Map
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
    { symbolTable = [empty]
    , tupleTable = empty
    , localOffset = 0
    }

type R = (Type, Location) -- id-associated-data in resolved ast
type T = Int -- tuple-associated-data in resolved ast
type SymbolTable = [Map String R]
type TupleTable = Map String [Decl ()]
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

addTupleDecl :: SourcePos -> String -> [Decl ()] -> ResolveM ()
addTupleDecl pos id_ decls = do
    tupTable <- tupleTable <$> get
    case Map.lookup id_ tupTable of
        Just _ -> lift . Left $ "Multiply declared tuple type: `" ++ id_ ++ "` @ " ++ show pos
        Nothing -> modify (\s -> s {tupleTable = insert id_ decls tupTable})

idLookup :: Id -> ResolveM (Maybe R)
idLookup id_ = do
    symTable <- symbolTable <$> get
    return $ case mapMaybe (Map.lookup id_) symTable of
        [] -> Nothing
        x:_ -> Just x

idLookupOrErr :: SourcePos -> Id -> ResolveM R
idLookupOrErr pos id_ = do
    maybeR <- idLookup id_
    case maybeR of
        Just r -> return r
        Nothing -> lift . Left $ "Undefined identifier: `" ++ id_ ++ "` @ " ++ show pos

tupleLookupOrErr :: Id -> ResolveM [Decl ()]
tupleLookupOrErr id_ = do
    tupTable <- tupleTable <$> get
    case Map.lookup id_ tupTable of
        Just decls -> return decls
        Nothing -> lift . Left $ "Undefined tuple type: `" ++ id_ ++ "`"

assertUniqueId :: SourcePos -> Id -> ResolveM ()
assertUniqueId pos id_ = do
    maybeMatch <- idLookup id_
    case maybeMatch of
        Just _ -> lift . Left $ "Multiply declared identifier: `" ++ id_ ++ "` @ " ++ show pos
        Nothing -> return ()

addGlobalDecl :: Decl () -> ResolveM ()
addGlobalDecl (Decl pos type_ id_) = addGlobalId pos (TValType type_) id_

addGlobalId :: SourcePos -> Type -> Id -> ResolveM ()
addGlobalId pos type_ id_ = do
    assertUniqueId pos id_
    let location = Label $ "_" ++ id_
    modifySymTable (\st -> insert id_ (type_, location) (head st) : tail st)

addLocalDecl :: Decl () -> ResolveM ()
addLocalDecl (Decl pos vType id_) = do
    assertUniqueId pos id_
    typeSize <- calcVTypeSize vType
    location <- allocateLocalSpace typeSize
    modifySymTable (\st -> insert id_ (TValType vType, location) (head st) : tail st)

calcVTypeSize :: ValueType () -> ResolveM Int
calcVTypeSize VTInteger = return 1
calcVTypeSize VTLogical = return 1
calcVTypeSize VTString = return 1 -- pointer
calcVTypeSize (VTTuple id_ ()) = do
    decls <- tupleLookupOrErr id_
    sum <$> mapM (\(Decl _pos vType _id) -> calcVTypeSize vType) decls

sizeVType :: ValueType () -> ResolveM (ValueType Int)
sizeVType t = do
    case t of
        VTInteger -> return VTInteger
        VTLogical -> return VTLogical
        VTString -> return VTString
        VTTuple id_ () -> VTTuple id_ <$> calcVTypeSize t

allocateLocalSpace :: Int -> ResolveM Location
allocateLocalSpace n = do
    oldCnt <- localOffset <$> get
    modify (\s -> s {localOffset = oldCnt + n})
    return . Offset $ oldCnt

resolveTopDecl :: TopDecl () () -> ResolveM (TopDecl R T)
resolveTopDecl (FnDecl pos retType id_ params body) = do
    let paramTypes = map (\(Decl _ type_ _ ) -> type_) params
    addGlobalId pos (TFn paramTypes retType) id_
    FnDecl pos retType id_ <$> mapM resolveDecl params <*> resolveBody body
resolveTopDecl (TupleDef pos id_ decls) = addTupleDecl pos id_ decls >> TupleDef pos id_ <$> mapM resolveDecl decls
resolveTopDecl (Global decl) = addGlobalDecl decl >> Global <$> resolveDecl decl

resolveDecl :: Decl () -> ResolveM (Decl T)
resolveDecl (Decl pos vType _id) = Decl pos <$> sizeVType vType <*> return _id

resolveBody :: Body () () -> ResolveM (Body R T)
resolveBody (decls, stmts) = withNewScope $ do
    mapM_ addLocalDecl decls
    stmts' <- mapM resolveStmt stmts
    decls' <- mapM resolveDecl decls
    return (decls', stmts')

resolveLvalue :: Lvalue () -> ResolveM (Lvalue R)
resolveLvalue (Identifier pos id_ ()) = do
    r <- idLookupOrErr pos id_
    return $ Identifier pos id_ r
resolveLvalue (TupleAccess pos lval id_ ()) = do
    rLval <- resolveLvalue lval
    let (tType, tLocation, _tPos) = case rLval of
            Identifier p _ (t, l) -> (t, l, p)
            TupleAccess p _ _ (t, l) -> (t, l, p)
    tupleTypeId <- case tType of
            (TValType (VTTuple x _tupSize)) -> return x
            _ -> lift . Left $ "Invalid access on non-tuple type `" ++ show tType ++ "` @ " ++ show pos
    decls <- tupleLookupOrErr tupleTypeId
    sizePrefixSum <- init . scanl (+) 0 <$> mapM (\(Decl _pos vType _id) -> calcVTypeSize vType) decls
    case filter ((\(Decl _pos _type declId) -> id_ == declId) . snd) $ zip sizePrefixSum decls of
        [] -> lift . Left $ "No such field `" ++ id_ ++ "` on tuple-type `" ++ tupleTypeId ++ "`"
        (inTupleOffset, Decl _pos declType _id):_ -> return . TupleAccess pos rLval id_ $ (TValType declType, addOffset inTupleOffset tLocation)

resolveStmt :: Stmt () () -> ResolveM (Stmt R T)
resolveStmt (Inc pos lval) = Inc pos <$> resolveLvalue lval
resolveStmt (Dec pos lval) = Dec pos <$> resolveLvalue lval
resolveStmt (While pos cond body) = While pos <$> resolveExpr cond <*> resolveBody body
resolveStmt (Read pos lval) = Read pos <$> resolveLvalue lval
resolveStmt (Write pos expr) = Write pos <$> resolveExpr expr
resolveStmt (Return pos maybeExpr) = Return pos <$> mapM resolveExpr maybeExpr
resolveStmt (ExprStmt pos expr) = ExprStmt pos <$> resolveExpr expr
resolveStmt (IfElse pos cond ifBody maybeElseBody) = IfElse pos <$> resolveExpr cond <*> resolveBody ifBody <*> mapM resolveBody maybeElseBody

resolveExpr :: Expr () -> ResolveM (Expr R)
resolveExpr (LogicalLit pos val) = return $ LogicalLit pos val
resolveExpr (IntLit pos val) = return $ IntLit pos val
resolveExpr (StringLit pos val) = return $ StringLit pos val
resolveExpr (Assignment pos lval expr) = Assignment pos <$> resolveLvalue lval <*> resolveExpr expr
resolveExpr (Call pos lval args) = Call pos <$> resolveLvalue lval <*> mapM resolveExpr args
resolveExpr (UnaryExpr pos op arg) = UnaryExpr pos op <$> resolveExpr arg
resolveExpr (BinaryExpr pos op left right) = BinaryExpr pos op <$> resolveExpr left <*> resolveExpr right
resolveExpr (Lvalue pos lval) = Lvalue pos <$> resolveLvalue lval
