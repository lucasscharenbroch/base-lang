module Ast where

type Id = String
type Decl = (ValueType, Id)
type Offset = Int

-- a = id-associated data
type Ast a = [TopDecl a]
type NoOffsetAst = Ast ()
type OffsetAst = Ast (ValueType, Offset)

data ValueType = VTInteger
               | VTLogical -- boolean
               | VTTuple Id

data TopDecl a = FnDecl Id [Decl] [Stmt a]
               | TupleDef Id [Decl]
               | Global Decl

data Stmt a = Inc (Expr a)
            | Dec (Expr a)
            | If (Expr a) [Stmt a]
            | IfElse (Expr a) [Stmt a] [Stmt a]
            | While (Expr a) [Stmt a]
            | Read (Expr a)
            | Write (Expr a)
            | Return (Maybe (Expr a))
            | ExprStmt (Expr a) -- call, assignment, etc.

data Expr a = LogicalLit Bool
            | IntLit Int
            | StringLit String
            | Assignment (Lvalue a) (Expr a)
            | Call (Lvalue a) [Expr a]
            | UnaryExpr UnaryOp (Expr a)
            | BinaryExpr BinaryOp (Expr a) (Expr a)
            | Lvalue (Lvalue a)

data Lvalue a = Identifier Id a
              | TupleAccess (Lvalue a) Id a

data UnaryOp = Negate | Not
data BinaryOp = Add | Sub | Mul | Div | Eq | Ne | Gt | Ge | Lt | Le | And | Or
