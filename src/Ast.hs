module Ast where

import Text.Parsec (SourcePos)

type Error = String

type Id = String
type Body a = ([Decl], [Stmt a])

data Decl = Decl SourcePos ValueType Id
    deriving (Show) -- TODO remove (all shows)

data Location = Label String
              | Offset Int
              | LabelPlusOffset String Int
    deriving (Show)

addOffset :: Int -> Location -> Location
addOffset o (Label s) = LabelPlusOffset s o
addOffset o (Offset o') = Offset $ o + o'
addOffset o (LabelPlusOffset s o') = LabelPlusOffset s $ o + o'

-- a = id-associated data
type Ast a = [TopDecl a]
type UnresolvedAst = Ast ()
type ResolvedAst = Ast (Type, Location)

data Type = TVoid
          | TFn [ValueType] Type
          | TValType ValueType
    deriving (Show, Eq)

data ValueType = VTInteger
               | VTLogical -- boolean
               | VTString
               | VTTuple Id
    deriving (Show, Eq)

data TopDecl a = FnDecl SourcePos Type Id [Decl] (Body a)
               | TupleDef SourcePos Id [Decl]
               | Global Decl
    deriving (Show)

data Stmt a = Inc SourcePos (Lvalue a)
            | Dec SourcePos (Lvalue a)
            | IfElse SourcePos (Expr a) (Body a) (Maybe (Body a))
            | While SourcePos (Expr a) (Body a)
            | Read SourcePos (Lvalue a)
            | Write SourcePos (Expr a)
            | Return SourcePos (Maybe (Expr a))
            | ExprStmt SourcePos (Expr a) -- call, assignment
    deriving (Show)

data Expr a = LogicalLit SourcePos Bool
            | IntLit SourcePos Int
            | StringLit SourcePos String
            | Assignment SourcePos (Lvalue a) (Expr a)
            | Call SourcePos (Lvalue a) [Expr a]
            | UnaryExpr SourcePos UnaryOp (Expr a)
            | BinaryExpr SourcePos BinaryOp (Expr a) (Expr a)
            | Lvalue SourcePos (Lvalue a)
    deriving (Show)

data Lvalue a = Identifier SourcePos Id a
              | TupleAccess SourcePos (Lvalue a) Id a
    deriving (Show)

data UnaryOp = Negate | Not
    deriving (Show)
data BinaryOp = Add | Sub | Mul | Div | Eq | Ne | Gt | Ge | Lt | Le | And | Or
    deriving (Show)
