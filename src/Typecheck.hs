module Typecheck where

import Ast
import Resolve (R)

import Text.Parsec (SourcePos)
import Data.List (intercalate)
import Data.Functor (void)

type CheckM = Either Error

expectType :: SourcePos -> Type -> Type -> CheckM ()
expectType pos t t'
    | t == t' = return ()
    | otherwise = Left $ "Type error: expected `" ++ show t ++ "`, got `" ++ show t' ++ "` @ " ++ show pos

expectOneOf :: SourcePos -> [Type] -> Type -> CheckM ()
expectOneOf pos ts t'
    | t' `elem` ts = return ()
    | otherwise = Left $ "Type error: expected one of " ++ tlist ++ ", got `" ++ show t' ++ "` @ " ++ show pos
    where tlist = "[" ++ intercalate "," (map (\t -> "`" ++ show t ++ "`") ts) ++ "]"

typeCheck :: ResolvedAst -> CheckM ()
typeCheck = mapM_ checkTopDecl

checkTopDecl :: TopDecl R -> CheckM ()
checkTopDecl (FnDecl _pos retType _id _params body) = checkBody retType body
checkTopDecl (TupleDef _ _ _) = return ()
checkTopDecl (Global _) = return ()

checkBody :: Type -> Body R -> CheckM ()
checkBody retType (_decls, stmts) = mapM_ (checkStmt retType) stmts

checkLvalue :: Lvalue R -> CheckM Type
checkLvalue (Identifier _pos _id (t, _loc)) = return t
checkLvalue (TupleAccess _pos _lval _id (t, _loc)) = return t

checkStmt :: Type -> Stmt R -> CheckM ()
checkStmt _ (Inc pos lval) = checkLvalue lval >>= expectType pos (TValType VTInteger)
checkStmt _ (Dec pos lval) = checkLvalue lval >>= expectType pos (TValType VTInteger)
checkStmt _ (Read pos lval) = checkLvalue lval >>= expectType pos (TValType VTInteger)
checkStmt _ (Write pos expr) = checkExpr expr >>= expectOneOf pos [TValType VTInteger, TValType VTLogical, TValType VTString]
checkStmt _ (ExprStmt _pos expr) = void $ checkExpr expr
checkStmt retType (IfElse pos cond body maybeBody) = do
    condType <- checkExpr cond
    expectType pos (TValType VTLogical) condType
    _ <- checkBody retType body
    mapM_ (checkBody retType) maybeBody
checkStmt retType (While pos cond body) = do
    condType <- checkExpr cond
    expectType pos (TValType VTLogical) condType
    void $ checkBody retType body
checkStmt retType (Return pos (Just expr)) = checkExpr expr >>= expectType pos retType
checkStmt retType (Return pos Nothing)
    | retType == TVoid = return ()
    | otherwise = Left $ "Expected return value of type `" ++ show retType ++ "` (got nothing) @ " ++ show pos

checkExpr :: Expr R -> CheckM Type
checkExpr (LogicalLit _ _) = return $ TValType VTLogical
checkExpr (IntLit _ _) = return $ TValType VTInteger
checkExpr (StringLit _ _) = return $ TValType VTString
checkExpr (Assignment pos lval expr) = expectType pos <$> checkLvalue lval <*> exprType >> exprType
    where exprType = checkExpr expr
checkExpr (Call pos lval args) = undefined -- TODO
checkExpr (UnaryExpr pos op expr) = undefined -- TODO
checkExpr (BinaryExpr pos op left right) = undefined -- TODO
checkExpr (Lvalue pos lval) = checkLvalue lval
