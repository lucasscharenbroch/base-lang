module Main where

import Typecheck
import Parse
import Resolve
import Generate
import Ast

import Control.Monad

main :: IO ()
main = putStrLn "Hello, Haskell!"

compile :: String -> Either Error MipsProgram
compile = parse >=>
          resolve >=>
          (\ast -> typeCheck ast >> return ast) >=>
          generate
