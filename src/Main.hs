module Main where

import Typecheck
import Parse
import Resolve
import Generate
import Ast

import System.IO

import Control.Monad

main :: IO ()
main = do
    input <- getContents
    case compile input of
        Left err -> hPutStrLn stderr err
        Right mipsProgram -> print mipsProgram

compile :: String -> Either Error MipsProgram
compile = parse >=>
          resolve >=>
          (\ast -> typeCheck ast >> return ast) >=>
          pure . generate
