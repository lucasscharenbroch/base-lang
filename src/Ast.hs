module Ast where

import Text.Parsec (SourcePos)

type Error = String

type Id = String
type Decl = (ValueType, Id)
type Offset = Int

-- a = id-associated data
type Ast a = [TopDecl a]
type UnresolvedAst = Ast ()
type ResolvedAst = Ast (ValueType, Offset)

data Type = TVoid
          | TValType ValueType

data ValueType = VTInteger
               | VTLogical -- boolean
               | VTString
               | VTTuple Id

data TopDecl a = FnDecl SourcePos Type Id [Decl] [Stmt a]
               | TupleDef SourcePos Id [Decl]
               | Global SourcePos Decl

data Stmt a = Inc SourcePos (Expr a)
            | Dec SourcePos (Expr a)
            | If SourcePos (Expr a) [Stmt a]
            | IfElse SourcePos (Expr a) [Stmt a] [Stmt a]
            | While SourcePos (Expr a) [Stmt a]
            | Read SourcePos (Lvalue a)
            | Write SourcePos (Expr a)
            | Return SourcePos (Maybe (Expr a))
            | ExprStmt SourcePos (Expr a) -- call, assignment, etc.

data Expr a = LogicalLit SourcePos Bool
            | IntLit SourcePos Int
            | StringLit SourcePos String
            | Assignment SourcePos (Lvalue a) (Expr a)
            | Call SourcePos (Lvalue a) [Expr a]
            | UnaryExpr SourcePos UnaryOp (Expr a)
            | BinaryExpr SourcePos BinaryOp (Expr a) (Expr a)
            | Lvalue SourcePos (Lvalue a)

data Lvalue a = Identifier SourcePos Id a
              | TupleAccess SourcePos (Lvalue a) Id a

data UnaryOp = Negate | Not
data BinaryOp = Add | Sub | Mul | Div | Eq | Ne | Gt | Ge | Lt | Le | And | Or
