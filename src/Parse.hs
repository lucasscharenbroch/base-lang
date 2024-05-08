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
stmt = undefined
