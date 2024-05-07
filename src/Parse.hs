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
fn = undefined

tupleDef :: Parser (TopDecl ())
tupleDef = undefined

global :: Parser (TopDecl ())
global = undefined
