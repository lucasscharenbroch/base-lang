module Generate where

import Ast
import Resolve (R)

type MipsProgram = [MipsSection]

data MipsSection = Text [Instruction]
                 | Data [DataDirective]

data Register = T0 | T1 | V0 | RA | SP | FP
type Label = String

data Instruction = TextLabel Label
                 | Comment String
                 | Commented Instruction
                 | StoreIdx Register Int Register -- sw $ra, 0($sp)
                 | LoadIdx Register Int Register -- lw $ra, 0($fp)
                 | StoreLabel Register Label -- sw $t0, _x
                 | LoadLabel Register Label -- lw $t0, _x
                 | SubtractUnsigned Register Register Int -- subu $sp, $sp, 4
                 | AddUnsigned Register Register Int -- addu $sp, $sp, 4
                 | Move Register Register -- move $t0, $t1
                 | Jump Register -- jr $ra
                 | JumpLabel Label -- j _f
                 | LoadImm Register Int -- li $v0, 1
                 | Syscall
                -- | -- TODO: all used instructions

data DataDirective = Align Int
                   | DataLabel Label
                   | Asciiz String

generate :: ResolvedAst -> MipsProgram
generate = concatMap genTopDecl

genTopDecl :: TopDecl R -> MipsProgram
genTopDecl = undefined

{-
data TopDecl a = FnDecl SourcePos Type Id [Decl] (Body a)
               | TupleDef SourcePos Id [Decl]
               | Global Decl
-}
