module Parse where

import Lex
import Ast

import Text.Parsec
import Text.Parsec.String
import Data.Bifunctor (first)

parse :: String -> Either Error NoOffsetAst
parse = first show . runParser program () "<input>"

program :: Parser NoOffsetAst
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
