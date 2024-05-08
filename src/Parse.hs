module Parse where

import Lex
import Ast

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity)

parse :: String -> Either Error UnresolvedAst
parse = first show . runParser program () "<input>"

program :: Parser UnresolvedAst
program = many topDecl <* eof

topDecl :: Parser (TopDecl ())
topDecl = fn
      <|> tupleDef
      <|> global

fn :: Parser (TopDecl ())
fn = try (FnDecl <$> getPosition <*> returnType <*> identifier <* symbol "{")
 <*> commaSep param <* symbol "}"
 <*> brackets body
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

local :: Parser Decl
local = try ((,) <$> valueType <*> identifier) <* dot

body :: Parser (Body ())
body = (,) <$> many local <*> many stmt

stmt :: Parser (Stmt ())
stmt = Inc <$> getPosition <*> expr <* dot
   <|> Dec <$> getPosition <*> expr <* dot
   <|> IfElse <$> getPosition <*> (reserved "if" *> expr) <*> brackets body <*> optionMaybe (reserved "else" *> brackets body)
   <|> While <$> getPosition <*> expr <*> brackets body
   <|> Read <$> getPosition <*> lvalue <* dot
   <|> Write <$> getPosition <*> expr <* dot
   <|> Return <$> getPosition <*> optionMaybe expr <* dot
   <|> ExprStmt <$> getPosition <*> exprStmt <* dot
   where exprStmt = call <|> assign

expr :: Parser (Expr ())
expr = buildExpressionParser [
            [prefix "~" Not, prefix "-" Negate],
            [binary "*" Mul AssocLeft, binary "/" Div AssocLeft],
            [binary "+" Add AssocLeft, binary "-" Sub AssocLeft],
            [binary "==" Eq AssocNone, binary ">" Gt AssocNone, binary ">=" Ge AssocNone,
             binary "~=" Ne AssocNone, binary "<" Lt AssocNone, binary "<=" Le AssocNone],
            [binary "&" And AssocLeft],
            [binary "|" Or AssocLeft]
      ] term

prefix :: String -> UnaryOp -> Operator String () Identity (Expr ())
prefix name op = Prefix (reservedOp name >> UnaryExpr <$> getPosition <*> return op)
binary :: String -> BinaryOp -> Assoc -> Operator String () Identity (Expr ())
binary name op = Infix (reservedOp name >> BinaryExpr <$> getPosition <*> return op)

call :: Parser (Expr ())
call = try (Call <$> getPosition <*> lvalue <* symbol "(") <*> commaSep expr <* symbol ")"

assign :: Parser (Expr ())
assign = try (Assignment <$> getPosition <*> lvalue <* reservedOp "=") <*> expr

term :: Parser (Expr ())
term = LogicalLit <$> getPosition <*> (True <$ reserved "True" <|> False <$ reserved "False")
   <|> IntLit <$> getPosition <*> (fromIntegral <$> natural)
   <|> StringLit <$> getPosition <*> stringLiteral
   <|> assign
   <|> call
   <|> Lvalue <$> getPosition <*> lvalue
   <|> parens expr

lvalue :: Parser (Lvalue ())
lvalue = intoLvalue . reverse <$> ((,) <$> getPosition <*> identifier) `sepBy1` colon
      where intoLvalue [(sp, i)] = Identifier sp i ()
            intoLvalue ((sp, i):rest) = TupleAccess sp (intoLvalue rest) i ()
