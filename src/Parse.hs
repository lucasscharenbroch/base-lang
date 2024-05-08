module Parse where

import Lex
import Ast

import Text.Parsec
import Text.Parsec.String
import Data.Bifunctor (first)

parse :: String -> Either Error UnresolvedAst
parse = first show . runParser program () "<input>"

program :: Parser UnresolvedAst
program = many topDecl

topDecl :: Parser (TopDecl ())
topDecl = fn
      <|> tupleDef
      <|> global

fn :: Parser (TopDecl ())
fn = try (FnDecl <$> getPosition <*> returnType <*> identifier <* symbol "{")
 <*> commaSep param <* symbol "}"
 <*> brackets (many stmt)
      where param = (,) <$> valueType <*> identifier

returnType :: Parser Type
returnType = TVoid <$ reserved "void"
         <|> TValType <$> valueType

valueType :: Parser ValueType
valueType = VTInteger <$ reserved "integer"
        <|> VTLogical <$ reserved "logical"
        <|> VTString <$ reserved "string"
        <|> VTTuple <$> (reserved "tuple" *> identifier)

tupleDef :: Parser (TopDecl ())
tupleDef = TupleDef <$> getPosition <*> try (reserved "tuple" *> identifier <* symbol "{")
       <*> many field <* symbol "}"
      where field = (,) <$> valueType <*> identifier <* dot

global :: Parser (TopDecl ())
global = Global <$> getPosition <*> try ((,) <$> valueType <*> identifier) <* dot

stmt :: Parser (Stmt ())
stmt = Inc <$> getPosition <*> expr <* dot
   <|> Dec <$> getPosition <*> expr <* dot
   <|> If <$> getPosition <*> expr <*> brackets (many stmt)
   <|> IfElse <$> getPosition <*> expr <*> brackets (many stmt) <*> brackets (many stmt)
   <|> While <$> getPosition <*> expr <*> brackets (many stmt)
   <|> Read <$> getPosition <*> lvalue <* dot
   <|> Write <$> getPosition <*> expr <* dot
   <|> Return <$> getPosition <*> optionMaybe expr <* dot
   <|> ExprStmt <$> getPosition <*> exprStmt <* dot
   where exprStmt = call <|> assign

expr :: Parser (Expr ())
expr = LogicalLit <$> getPosition <*> (True <$ reserved "True" <|> False <$ reserved "False")
   <|> IntLit <$> getPosition <*> (fromIntegral <$> natural)
   <|> StringLit <$> getPosition <*> stringLiteral
   <|> UnaryExpr <$> getPosition <*> unaryOp <*> expr
   <|> try ((\sp x o y -> BinaryExpr sp o x y) <$> getPosition <*> expr <*> binaryOp) <*> expr
   <|> assign
   <|> call
   <|> Lvalue <$> getPosition <*> lvalue

call :: Parser (Expr ())
call = try (Call <$> getPosition <*> lvalue <* symbol "(") <*> commaSep expr <* symbol ")"

assign :: Parser (Expr ())
assign = try (Assignment <$> getPosition <*> lvalue <* reservedOp "=") <*> expr

unaryOp :: Parser UnaryOp
unaryOp = Negate <$ reservedOp "-"
      <|> Not <$ reservedOp "~"

binaryOp :: Parser BinaryOp
binaryOp = Add <$ reservedOp "+"
       <|> Sub <$ reservedOp "-"
       <|> Mul <$ reservedOp "*"
       <|> Div <$ reservedOp "/"
       <|> Eq <$ reservedOp "=="
       <|> Ne <$ reservedOp "~="
       <|> Gt <$ reservedOp ">"
       <|> Ge <$ reservedOp ">="
       <|> Lt <$ reservedOp "<"
       <|> Le <$ reservedOp "<="
       <|> And <$ reservedOp "&"
       <|> Or <$ reservedOp "|"

lvalue :: Parser (Lvalue ())
lvalue = intoLvalue . reverse <$> ((,) <$> getPosition <*> identifier) `sepBy1` colon
      where intoLvalue [(sp, i)] = Identifier sp i ()
            intoLvalue ((sp, i):rest) = TupleAccess sp (intoLvalue rest) i ()
