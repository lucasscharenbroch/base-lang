module Lex where

import Error

data Token = Token { val :: TokVal
                   , lineNum :: Int
                   , charNum :: Int
                   }

data TokVal = Void | Logical | Integer | Tuple | True | False
            | Read | Write | If | Else | While | Return
            | Id String | IntLit Int | StrLit String
            | LCurly | RCurly | LParen | RParen | LBracket | RBracket
            | Colon | Comma | Period | LeftShift | RightShift
            | Eq | Tilde | Ampersand | Pipe | Inc | Dec
            | Plus | Minus | Asterisk | Slash
            | Lt | Gt | Le | Ge | EqEq | Ne

lex :: String -> Either Error [Token]
lex = undefined
