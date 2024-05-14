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

topDecl :: Parser (TopDecl () ())
topDecl = fn
      <|> tupleDef
      <|> global

fn :: Parser (TopDecl () ())
fn = try (FnDecl <$> getPosition <*> returnType <*> identifier <* symbol "{")
 <*> commaSep decl <* symbol "}"
 <*> brackets body
 <*> return ()

returnType :: Parser (Type ())
returnType = TVoid <$ reserved "void"
         <|> TValType <$> valueType

valueType :: Parser (ValueType ())
valueType = VTInteger <$ reserved "integer"
        <|> VTLogical <$ reserved "logical"
        <|> VTString <$ reserved "string"
        <|> VTTuple <$> (reserved "tuple" *> identifier) <*> return ()

tupleDef :: Parser (TopDecl () ())
tupleDef = TupleDef <$> getPosition <*> try (reserved "tuple" *> identifier <* symbol "{")
       <*> many declDot <* symbol "}"
       <* dot

global :: Parser (TopDecl () ())
global = Global <$> declDot

decl :: Parser (Decl ())
decl = Decl <$> getPosition <*> valueType <*> identifier

declDot :: Parser (Decl ())
declDot = decl <* dot

body :: Parser (Body () ())
body = (,) <$> many declDot <*> many stmt

stmt :: Parser (Stmt () ())
stmt = IfElse <$> getPosition <*> (reserved "if" *> expr) <*> brackets body <*> optionMaybe (reserved "else" *> brackets body)
   <|> While <$> getPosition <*> (reserved "while" *> expr) <*> brackets body
   <|> Read <$> getPosition <*> (reserved "read" *> reservedOp ">>" *> lvalue) <* dot
   <|> Write <$> getPosition <*> (reserved "write" *> reservedOp "<<" *> expr) <* dot
   <|> Return <$> getPosition <*> (reserved "return" *> optionMaybe expr) <* dot
   <|> ExprStmt <$> getPosition <*> (call <|> assign) <* dot
   <|> do pos <- getPosition
          lval <- lvalue
          ((Inc pos lval  <$ reservedOp "++") <|> (Dec pos lval <$ reservedOp "--")) <* dot

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
            intoLvalue _ = error "Internal: sepBy1 yeilds empty list"
