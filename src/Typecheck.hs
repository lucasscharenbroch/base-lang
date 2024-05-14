module Typecheck where

import Ast
import Resolve (R, T)

import Text.Parsec (SourcePos)
import Data.List (intercalate)
import Data.Functor (void)
import Control.Monad (join)

type CheckM = Either Error

expectType_ :: SourcePos -> Type T -> Type T -> CheckM ()
expectType_ = ((.) . (.) . (.)) void expectType

expectType :: SourcePos -> Type T -> Type T -> CheckM (Type T)
expectType pos t t'
    | t == t' = return t
    | otherwise = Left $ "Type error: expected `" ++ show t ++ "`, got `" ++ show t' ++ "` @ " ++ show pos

expectOneOf :: SourcePos -> [Type T] -> Type T -> CheckM ()
expectOneOf pos ts t'
    | t' `elem` ts = return ()
    | otherwise = Left $ "Type error: expected one of " ++ tlist ++ ", got `" ++ show t' ++ "` @ " ++ show pos
    where tlist = "[" ++ intercalate "," (map (\t -> "`" ++ show t ++ "`") ts) ++ "]"

typeCheck :: ResolvedAst -> CheckM ()
typeCheck = mapM_ checkTopDecl

checkTopDecl :: TopDecl R T -> CheckM ()
checkTopDecl (FnDecl _pos retType _id _params body _numLocals) = checkBody retType body
checkTopDecl (TupleDef _ _ _) = return ()
checkTopDecl (Global _) = return ()

checkBody :: Type T -> Body R T -> CheckM ()
checkBody retType (_decls, stmts) = mapM_ (checkStmt retType) stmts

checkLvalue :: Lvalue R -> CheckM (Type T)
checkLvalue (Identifier _pos _id (t, _loc)) = return t
checkLvalue (TupleAccess _pos _lval _id (t, _loc)) = return t

checkStmt :: Type T -> Stmt R T -> CheckM ()
checkStmt _ (Inc pos lval) = checkLvalue lval >>= expectType_ pos (TValType VTInteger)
checkStmt _ (Dec pos lval) = checkLvalue lval >>= expectType_ pos (TValType VTInteger)
checkStmt _ (Read pos lval) = checkLvalue lval >>= expectType_ pos (TValType VTInteger)
checkStmt _ (Write pos expr) = checkExpr expr >>= expectOneOf pos [TValType VTInteger, TValType VTLogical, TValType VTString]
checkStmt _ (ExprStmt _pos expr) = void $ checkExpr expr
checkStmt retType (IfElse pos cond body maybeBody) = do
    condType <- checkExpr cond
    expectType_ pos (TValType VTLogical) condType
    _ <- checkBody retType body
    mapM_ (checkBody retType) maybeBody
checkStmt retType (While pos cond body) = do
    condType <- checkExpr cond
    expectType_ pos (TValType VTLogical) condType
    void $ checkBody retType body
checkStmt retType (Return pos (Just expr)) = checkExpr expr >>= expectType_ pos retType
checkStmt retType (Return pos Nothing)
    | retType == TVoid = return ()
    | otherwise = Left $ "Expected return value of type `" ++ show retType ++ "` (got nothing) @ " ++ show pos

checkExpr :: Expr R -> CheckM (Type T)
checkExpr (LogicalLit _ _) = return $ TValType VTLogical
checkExpr (IntLit _ _) = return $ TValType VTInteger
checkExpr (StringLit _ _) = return $ TValType VTString
checkExpr (Assignment pos lval expr) = join $ expectType pos <$> checkLvalue lval <*> exprType
    where exprType = checkExpr expr
checkExpr (Call pos lval args) = do
    lvalType <- checkLvalue lval
    argTypes <- mapM checkExpr args
    case lvalType of
        TFn paramValueTypes retType
            | nGiven /= nExpected -> Left $ "Wrong number of arguments to function `" ++ show lval ++ "` :" ++
                                            "got " ++ show nGiven ++ ", expected " ++ show nExpected ++ " @ " ++ show pos
            | or $ zipWith (/=) argTypes paramTypes -> Left $ "Misaligned argument types: expected " ++ show paramTypes ++
                                                              ", got " ++ show argTypes ++ " @ " ++ show pos
            | otherwise -> return retType
            where nGiven = length argTypes
                  nExpected = length paramValueTypes
                  paramTypes = map TValType paramValueTypes
        _ -> Left $ "Invalid call of non-function lvalue `" ++ show lval ++
                    "`, of type `" ++ show lvalType ++ "` @ " ++ show pos
checkExpr (UnaryExpr pos op expr) = checkExpr expr >>= checkUnaryOp pos op
checkExpr (BinaryExpr pos op left right) = do
    left' <- checkExpr left
    right' <- checkExpr right
    checkBinaryOp pos op left' right'
checkExpr (Lvalue _pos lval) = checkLvalue lval

checkBinaryOp :: SourcePos -> BinaryOp -> Type T -> Type T -> CheckM (Type T)
checkBinaryOp pos op left right = case op of
    Add -> checkArith
    Sub -> checkArith
    Mul -> checkArith
    Div -> checkArith
    Eq -> checkCmp
    Ne -> checkCmp
    Gt -> checkCmp
    Ge -> checkCmp
    Lt -> checkCmp
    Le -> checkCmp
    And -> checkLogical
    Or -> checkLogical
    where comparable = [int, bool, str]
          int = TValType VTInteger
          bool = TValType VTLogical
          str = TValType VTString
          checkArith = mapM_ (expectType pos int) [left, right] >> return int
          checkCmp = expectOneOf pos comparable left >> expectType pos left right >> return bool
          checkLogical = mapM_ (expectType pos bool) [left, right] >> return bool

checkUnaryOp :: SourcePos -> UnaryOp -> Type T -> CheckM (Type T)
checkUnaryOp pos op arg = case op of
    Negate -> expectType pos (TValType VTInteger) arg >> return arg
    Not -> expectType pos (TValType VTLogical) arg >> return arg
