module Lex where

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = makeTokenParser $ Tok.LanguageDef
    { commentStart = ""
    , commentEnd = ""
    , commentLine = "" -- there are two ways to have a comment line: (!!) and ($)
                       -- handle these manually
    , nestedComments = False
    , identStart = letter <|> char '_'
    , identLetter = letter <|> digit <|> char '_'
    , opStart = oneOf ""
    , opLetter = oneOf "+-*/&|~<>="
    , reservedNames = ["void", "logical", "integer", "True", "False", "tuple",
                       "read", "write", "if", "else", "while", "return"]
    , reservedOpNames = []
    , caseSensitive = True
    }

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

natural :: Parser Integer
natural = Tok.natural lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

dot :: Parser ()
dot = void $ Tok.dot lexer

colon :: Parser ()
colon = void $ Tok.colon lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

symbol :: String -> Parser ()
symbol = void . Tok.symbol lexer
