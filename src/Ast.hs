module Ast where

import Text.Parsec (SourcePos)

type Error = String
type Id = String
type Body i t = ([Decl t], [Stmt i t])

data Decl t = Decl SourcePos (ValueType t) Id

data Location = Label String
              | LocalOffset Int
              | ParamOffset Int
              | LabelPlusOffset String Int
    deriving (Show)

addOffset :: Int -> Location -> Location
addOffset o (Label s) = LabelPlusOffset s o
addOffset o (LocalOffset o') = LocalOffset $ o + o'
addOffset o (ParamOffset o') = ParamOffset $ o + o'
addOffset o (LabelPlusOffset s o') = LabelPlusOffset s $ o + o'

-- i = id-associated data
-- t = type-size data (tuple types, function declarations)
type Ast i t = [TopDecl i t]
type UnresolvedAst = Ast () ()
type ResolvedAst = Ast (Type Int, Location) Int

data Type t = TVoid
          | TFn [ValueType t] (Type t)
          | TValType (ValueType t)
    deriving (Show, Eq)

data ValueType t = VTInteger
                 | VTLogical -- boolean
                 | VTString
                 | VTTuple Id t
    deriving (Show, Eq)

data TopDecl i t = FnDecl SourcePos (Type t) Id [Decl t] (Body i t) t
                 | TupleDef SourcePos Id [Decl t]
                 | Global (Decl t)

data Stmt i t = Inc SourcePos (Lvalue i)
              | Dec SourcePos (Lvalue i)
              | IfElse SourcePos (Expr i) (Body i t) (Maybe (Body i t))
              | While SourcePos (Expr i) (Body i t)
              | Read SourcePos (Lvalue i)
              | Write SourcePos (Expr i)
              | Return SourcePos (Maybe (Expr i))
              | ExprStmt SourcePos (Expr i) -- call, assignment

data Expr i = LogicalLit SourcePos Bool
            | IntLit SourcePos Int
            | StringLit SourcePos String
            | Assignment SourcePos (Lvalue i) (Expr i)
            | Call SourcePos (Lvalue i) [Expr i]
            | UnaryExpr SourcePos UnaryOp (Expr i)
            | BinaryExpr SourcePos BinaryOp (Expr i) (Expr i)
            | Lvalue SourcePos (Lvalue i)

data Lvalue i = Identifier SourcePos Id i
              | TupleAccess SourcePos (Lvalue i) Id i
    deriving (Show)

data UnaryOp = Negate | Not
data BinaryOp = Add | Sub | Mul | Div | Eq | Ne | Gt | Ge | Lt | Le | And | Or
