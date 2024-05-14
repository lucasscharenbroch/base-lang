module Generate where

import Ast
import Resolve (R, T)

import Data.List (intercalate, singleton)
import Data.Maybe (fromMaybe)
import Text.Parsec (SourcePos)
import Control.Monad.State.Lazy

data GenState = GenState
    { labelStream :: [Label]
    , dataDirectives :: [DataDirective]
    }

initialGenState :: GenState
initialGenState = GenState
    { labelStream = map (("L"++) . show) [0..]
    , dataDirectives = []
    }

type MipsProgram = ([Instruction], [DataDirective])
type GenM = State GenState
type Label = String

data Register = T0 | T1 | V0 | RA | SP | FP | A0

instance Show Register where
    show T0 = "$t0"
    show T1 = "$t1"
    show V0 = "$v0"
    show RA = "$ra"
    show SP = "$sp"
    show FP = "$fp"
    show A0 = "$a0"

infixr 5 +>+, +&+, +$+

(+>+) :: Monad m => m [a] -> m [a] -> m [a]
xs +>+ ys = (++) <$> xs <*> ys

(+&+) :: Monad m => m [a] -> [a] -> m [a]
xs +&+ ys = (++) <$> xs <*> return ys

(+$+) :: Monad m => [a] -> m [a] -> m [a]
xs +$+ ys = (++xs) <$> ys

data Instruction = TextLabel Label
                 | MainFnLabel
                 | Comment String
                 | Commented Instruction String
                 | StoreIdx Register Int Register -- sw $ra, 0($sp)
                 | LoadIdx Register Int Register -- lw $ra, 0($fp)
                 | StoreLabel Register Label -- sw $t0, _x
                 | LoadLabel Register Label -- lw $t0, _x
                 | SubUnsigned Register Register Int -- subu $sp, $sp, 4
                 | AddUnsigned Register Register Int -- addu $sp, $sp, 4
                 | Move Register Register -- move $t0, $t1
                 | Jump Register -- jr $ra
                 | JumpLabel Label -- j _L0
                 | Call Register -- jalr $t0
                 | LoadImm Register Int -- li $v0, 1
                 | AddImm Register Register Int -- addi $t0, $t0, 1
                 | BranchEqZ Register Label -- beqz $t0, _L14
                 | BranchNeZ Register Label -- bnez $t0, _L14
                 | LoadAddress Register Label -- la $t0, _g
                 | LoadAddressPlusOffset Register Label Int -- la $t0, (_g + 1)
                 | LoadAddressIdx Register Int Register -- la $t0, 0($t1)
                 | SetEq Register Register Register -- seq $t0, $t0, $t1
                 | SetNe Register Register Register -- sne $t0, $t0, $t1
                 | SetGt Register Register Register -- sgt $t0, $t0, $t1
                 | SetGe Register Register Register -- sge $t0, $t0, $t1
                 | SetLt Register Register Register -- slt $t0, $t0, $t1
                 | SetLe Register Register Register -- sle $t0, $t0, $t1
                 | Negate Register Register -- neg $t0, $t0
                 | Add Register Register Register -- add $t0, $t0, $t1
                 | Sub Register Register Register -- sub $t0, $t0, $t1
                 | Mul Register Register Register -- mul $t0, $t0, $t1
                 | Div Register Register Register -- div $t0, $t0, $t1
                 | Push Register -- lw $r0 0($sp) \n subu $sp $sp 4
                 | Pop Register -- lw $r0 4($sp) \n addu $sp $sp 4
                 | Pop_ -- addu $sp $sp 4
                 | PushN Location Int
                 | PopN Location Int
                 | PopN_ Int
                 | Syscall -- syscall
                 | MainReturn -- li $v0 10 \n syscall
                 | RotateStack Int Int

instance Show Instruction where
    show (TextLabel l) = l ++ ":"
    show MainFnLabel = ".globl main\nmain:"
    show (Comment c) = "# " ++ c
    show (Commented i c) = show i ++ "# " ++ c
    show (StoreIdx r0 offset r1) = "sw " ++ show r0 ++ ", " ++ show offset ++ "(" ++ show r1 ++ ")"
    show (LoadIdx r0 offset r1) = "lw " ++ show r0 ++ ", " ++ show offset ++ "(" ++ show r1 ++ ")"
    show (StoreLabel r l) = "sw " ++ show r ++ ", " ++ l
    show (LoadLabel r l) = "lw " ++ show r ++ ", " ++ l
    show (SubUnsigned r0 r1 i) = "subu " ++ show r0 ++ ", " ++ show r1 ++ ", " ++ show i
    show (AddUnsigned r0 r1 i) = "addu " ++ show r0 ++ ", " ++ show r1 ++ ", " ++ show i
    show (Move r0 r1) = "move " ++ show r0 ++ ", " ++ show r1
    show (Jump r) = "jr " ++ show r
    show (JumpLabel l) = "j " ++ l
    show (Generate.Call r) = "jalr " ++ show r
    show (LoadImm r i) = "li " ++ show r ++ ", " ++ show i
    show (AddImm r0 r1 i) = "addi " ++ show r0 ++ ", " ++ show r1 ++ ", " ++ show i
    show (BranchEqZ r l) = "beqz " ++ show r ++ ", " ++ l
    show (BranchNeZ r l) = "bnez " ++ show r ++ ", " ++ l
    show (LoadAddress r l) = "la " ++ show r ++ ", " ++ l
    show (LoadAddressPlusOffset r l o) = "la " ++ show r ++ ", (" ++ l ++ " + " ++ show o ++ ")"
    show (LoadAddressIdx r0 offset r1) = "la " ++ show r0 ++ ", " ++ show offset ++ "(" ++ show r1 ++ ")"
    show (SetEq r0 r1 r2) = "seq " ++ intercalate ", " (map show [r0, r1, r2])
    show (SetNe r0 r1 r2) = "sne " ++ intercalate ", " (map show [r0, r1, r2])
    show (SetGt r0 r1 r2) = "sgt " ++ intercalate ", " (map show [r0, r1, r2])
    show (SetGe r0 r1 r2) = "sge " ++ intercalate ", " (map show [r0, r1, r2])
    show (SetLt r0 r1 r2) = "slt " ++ intercalate ", " (map show [r0, r1, r2])
    show (SetLe r0 r1 r2) = "sle " ++ intercalate ", " (map show [r0, r1, r2])
    show (Generate.Add r0 r1 r2) = "add " ++ intercalate ", " (map show [r0, r1, r2])
    show (Generate.Sub r0 r1 r2) = "sub " ++ intercalate ", " (map show [r0, r1, r2])
    show (Generate.Mul r0 r1 r2) = "mul " ++ intercalate ", " (map show [r0, r1, r2])
    show (Generate.Div r0 r1 r2) = "div " ++ intercalate ", " (map show [r0, r1, r2])
    show (Generate.Negate r0 r1) = "neg " ++ show r0 ++ ", " ++ show r1
    show (Push r) = show (Commented (LoadIdx r 0 SP) ("Push " ++ show r)) ++ "\n" ++ show (SubUnsigned  SP SP 4)
    show (Pop r) = show (Commented (LoadIdx r 4 SP) ("Pop " ++ show r)) ++ "\n" ++ show (AddUnsigned  SP SP 4)
    show Pop_ = show (Commented (AddUnsigned SP SP 4) "Pop _")
    show (PushN location n) = intercalate "\n" [show $ genAddrFromLocation (addOffset i location) ++ [Pop T0, LoadIdx T0 0 T0, Push T0] | i <- [0..n-1]]
    show (PopN location n) = intercalate "\n" [show $ genAddrFromLocation (addOffset i location) ++ [Pop T0, Pop T1, StoreIdx T1 0 T0] | i <- [0..n-1]]
    show (PopN_ n) = show (Commented (AddUnsigned SP SP (4 * n)) "Pop ") ++ show n ++ " _"
    show Syscall = "syscall"
    show MainReturn = intercalate "\n" . map show $ [LoadImm V0 10, Syscall]
    show (RotateStack n sz)
        | sz == 0 || n `mod` sz == 0 = []
        | otherwise = intercalate "\n" . map show $ instrs
        where idxs = take sz [(n * i) `mod` sz | i <- [0..]]
              offsets = [sz - (i * 4) | i <- idxs] -- from $sp
              instrs = [LoadIdx T1 (last offsets) SP] ++
                       concatMap (\o -> [LoadIdx T0 o SP, StoreIdx T1 o SP, Move T0 T1]) offsets

data DataDirective = Align Int
                   | DataLabel Label
                   | Asciiz String
                   | Space Int

instance Show DataDirective where
    show (Align n) = ".align" ++ show n
    show (DataLabel l) = l ++ ":"
    show (Asciiz s) = ".asciiz " ++ show s
    show (Space n) = ".space" ++ show n

freshLabel :: GenM Label
freshLabel = (head . labelStream <$> get) <* modify (\s -> s { labelStream = tail . labelStream $ s })

addData :: [DataDirective] -> GenM ()
addData dds = modify (\s -> s { dataDirectives = dataDirectives s ++ dds })

vTypeSizeInBytes :: ValueType T -> Int
vTypeSizeInBytes (VTTuple _ sz) = sz * 4
vTypeSizeInBytes _ = 4

typeSizeInBytes :: Type T -> Int
typeSizeInBytes (TValType vt) = vTypeSizeInBytes vt
typeSizeInBytes _ = 0

generate :: ResolvedAst -> MipsProgram
generate ast = (concat instructionss, dataDirectives resState)
    where (instructionss, resState) = runState (mapM genTopDecl ast) initialGenState

genGlobal :: Decl T -> GenM ()
genGlobal (Decl _pos valType id_) = addData [Align 2, DataLabel $ "_" ++ id_, Space $ vTypeSizeInBytes valType]

genTopDecl :: TopDecl R T -> GenM [Instruction]
genTopDecl (TupleDef _ _ _) = return []
genTopDecl (Global decl) = genGlobal decl >> return []
genTopDecl (FnDecl _pos retType id_ _paramDecls body numLocalBytes) = do
    generatedBody <- genBody exitLabel body
    return $ [
            -- "Preamble"
            fnLabel,
            -- "Prelude"
            Push RA,
            Push FP,
            AddUnsigned FP SP 8,
            SubUnsigned SP SP localsSize,
            -- Body
            Comment $ "Begin function body " ++ id_
        ] ++ generatedBody ++ [
            Comment $ "End function body " ++ id_,
            -- Exit
            TextLabel exitLabel,
            LoadIdx RA 0 FP,
            Move T0 FP,
            LoadIdx FP (-4) FP,
            RotateStack returnSize (2 + localsSize + returnSize),
            PopN_ (2 + localsSize),
            returnInstruction
        ]
    where fnLabel
              | id_ == "main" = MainFnLabel
              | otherwise = TextLabel $ "_" ++ id_
          exitLabel = "_" ++ id_ ++ "_EXIT"
          localsSize = 4 * numLocalBytes
          returnSize = typeSizeInBytes retType
          returnInstruction
              | id_ == "main" = MainReturn
              | otherwise = Jump RA

genMutImmAdd :: SourcePos -> Int -> Lvalue R -> GenM [Instruction]
genMutImmAdd pos n lval = genExpr (Lvalue pos lval) +&+ genAddr lval ++ [
        Pop T0,
        Pop T1,
        AddImm T1 T1 n,
        StoreIdx T1 0 T0
    ]

genBody :: Label -> Body R T -> GenM [Instruction]
genBody retLabel = pure . concat <=< mapM (genStmt retLabel) . snd

genStmt :: Label -> Stmt R T -> GenM [Instruction]
genStmt _ (Inc pos lval) = genMutImmAdd pos 1 lval
genStmt _ (Dec pos lval) = genMutImmAdd pos (-1) lval
genStmt retLabel (IfElse _pos cond body maybeBody) = do
    elseLabel <- freshLabel
    doneLabel <- freshLabel
    thenBody <- genBody retLabel body
    elseBody <- fromMaybe [] <$> mapM (genBody retLabel) maybeBody
    genExpr cond +&+
        [Pop T0, BranchEqZ T0 elseLabel] ++
        thenBody ++
        [JumpLabel doneLabel, Commented (TextLabel doneLabel) "Else"] ++
        elseBody ++
        [Commented (TextLabel doneLabel) "Done"]
genStmt retLabel (While _pos cond body) = do
    beginLabel <- freshLabel
    doneLabel <- freshLabel
    generatedBody <- genBody retLabel body
    return [Commented (TextLabel beginLabel) "Begin while"] +>+
           genExpr cond +&+
           [Pop T0, BranchEqZ T0 doneLabel] ++
           generatedBody ++
           [Commented (TextLabel doneLabel) "End while"]
genStmt _ (Read _pos lval) = return $ [LoadImm V0 5, Syscall] ++ genAddr lval ++ [Pop T0, StoreIdx V0 0 T0]
genStmt _ (Write _pos expr) = genExpr expr +&+ [Pop A0, LoadImm V0 printType, Syscall]
    where printType = case getExprType expr of
              TValType VTInteger -> 1
              TValType VTString -> 4
              _ -> error "Internal: bad type for write"
genStmt _ (ExprStmt _pos expr) = genExpr expr +&+ [PopN_ numWords]
    where numWords = typeSizeInBytes (getExprType expr) `div` 4
genStmt retLabel (Return _pos (Just expr)) = genExpr expr +&+ [JumpLabel retLabel]
genStmt retLabel (Return _pos Nothing) = return [JumpLabel retLabel]

getLvalueLocation :: Lvalue R -> Location
getLvalueLocation (Identifier _ _ (_, x)) = x
getLvalueLocation (TupleAccess _ _ _ (_, x)) = x

getLvalueType :: Lvalue R -> Type T
getLvalueType (Identifier _ _ (x, _)) = x
getLvalueType (TupleAccess _ _ _ (x, _)) = x

getExprType :: Expr R -> Type T
getExprType (LogicalLit _ _) = TValType VTLogical
getExprType (IntLit _ _) = TValType VTInteger
getExprType (StringLit _ _) = TValType VTString
getExprType (Assignment _ _ rhs) = getExprType rhs
getExprType (Ast.Call _ lval _) = case getLvalueType lval of
    (TFn _ retType) -> retType
    _ -> error "Internal: non-function call"
getExprType (UnaryExpr _ op _) = case op of
    Not -> TValType VTLogical
    Ast.Negate -> TValType VTInteger
getExprType (BinaryExpr _ op _ _) = case op of
    Ast.Add -> TValType VTInteger
    Ast.Sub -> TValType VTInteger
    Ast.Mul -> TValType VTInteger
    Ast.Div -> TValType VTInteger
    Eq -> TValType VTLogical
    Ne -> TValType VTLogical
    Gt -> TValType VTLogical
    Ge -> TValType VTLogical
    Lt -> TValType VTLogical
    Le -> TValType VTLogical
    And -> TValType VTLogical
    Or -> TValType VTLogical
getExprType (Lvalue _ lval) = getLvalueType lval

getLvalueNumWords :: Lvalue R -> Int
getLvalueNumWords (Identifier _ _ ((TValType vType), _)) = vTypeSizeInBytes vType `div` 4
getLvalueNumWords (TupleAccess _ _ _ ((TValType vType), _)) = vTypeSizeInBytes vType `div` 4
getLvalueNumWords _ = 0

genAddr :: Lvalue R -> [Instruction]
genAddr lval = genAddrFromLocation $ getLvalueLocation lval

genAddrFromLocation :: Location -> [Instruction]
genAddrFromLocation (Label label) = [LoadAddress T0 label, Push T0]
genAddrFromLocation (LabelPlusOffset label offset) = [LoadAddressPlusOffset T0 label offset, Push T0]
genAddrFromLocation (LocalOffset nWords) = [LoadAddressIdx T0 (-8 + nWords * (-4)) FP]
genAddrFromLocation (ParamOffset nWords) = [LoadAddressIdx T0 (4 + nWords * 4) FP]

genExpr :: Expr R -> GenM [Instruction]
genExpr (LogicalLit _pos bool) = return [LoadImm T0 (fromEnum bool), Push T0]
genExpr (IntLit _pos int) = return [LoadImm T0 int, Push T0]
genExpr (StringLit _pos str) = do
    strLabel <- freshLabel
    addData [DataLabel strLabel, Asciiz str]
    return [LoadAddress T0 strLabel, Push T0]
genExpr (Assignment _pos lval expr) = genExpr expr +&+ [PopN lvalLoc lvalSz, PushN lvalLoc lvalSz]
    where lvalLoc = getLvalueLocation lval
          lvalSz = getLvalueNumWords lval
genExpr (Ast.Call _pos lval args) = (concat <$> mapM genExpr args) +&+ genAddr lval ++
                                    [Pop T0, Generate.Call T0,
                                     RotateStack retWords (argsWords + retWords), PopN_ argsWords]
    where argsWords = (`div`4) . sum . map vTypeSizeInBytes $ paramTypes
          retWords = typeSizeInBytes retType `div` 4
          (paramTypes, retType) = case getLvalueType lval of
              TFn params ret -> (params, ret)
              _ -> error "Internal: call on non-function lvalue"
genExpr (UnaryExpr _pos op expr) = genUnaryOp op expr
genExpr (BinaryExpr _pos op left right) = genBinaryOp op left right
genExpr (Lvalue _pos lval) = return [PopN (getLvalueLocation lval) (getLvalueNumWords lval)]

genUnaryOp :: UnaryOp -> Expr R -> GenM [Instruction]
genUnaryOp Ast.Negate expr = genUnaryOp' expr [Generate.Negate T0 T0]
genUnaryOp Not expr = genUnaryOp' expr [LoadImm T1 0, SetEq T0 T0 T1]

-- argument in T0, return in T0
genUnaryOp' :: Expr R -> [Instruction] -> GenM [Instruction]
genUnaryOp' expr body = genExpr expr +&+ [Pop T0] ++ body ++ [Push T0]

genBinaryOp :: BinaryOp -> Expr R -> Expr R -> GenM [Instruction]
genBinaryOp Ast.Add left right = genBinaryOp' left right [Generate.Add T0 T0 T1]
genBinaryOp Ast.Sub left right = genBinaryOp' left right [Generate.Sub T0 T0 T1]
genBinaryOp Ast.Mul left right = genBinaryOp' left right [Generate.Mul T0 T0 T1]
genBinaryOp Ast.Div left right = genBinaryOp' left right [Generate.Div T0 T0 T1]
genBinaryOp Eq left right = genBinaryOp' left right [Generate.SetEq T0 T0 T1]
genBinaryOp Ne left right = genBinaryOp' left right [Generate.SetNe T0 T0 T1]
genBinaryOp Gt left right = genBinaryOp' left right [Generate.SetGt T0 T0 T1]
genBinaryOp Ge left right = genBinaryOp' left right [Generate.SetGe T0 T0 T1]
genBinaryOp Lt left right = genBinaryOp' left right [Generate.SetLt T0 T0 T1]
genBinaryOp Le left right = genBinaryOp' left right [Generate.SetLe T0 T0 T1]
genBinaryOp And left right = genShortCircuited BranchEqZ left right
genBinaryOp Or left right = genShortCircuited BranchNeZ left right

-- arguments in T0 and T1, return in T0
genBinaryOp' :: Expr R -> Expr R -> [Instruction] -> GenM [Instruction]
genBinaryOp' left right body = genExpr left +>+ genExpr right +&+ [Pop T1, Pop T0] ++ body ++ [Push T0]

genShortCircuited :: (Register -> Label -> Instruction) -> Expr R -> Expr R -> GenM [Instruction]
genShortCircuited condition left right = do
    doneLabel <- freshLabel
    genExpr left +>+
        [Pop T0, condition T0 doneLabel] +$+
        genExpr right +&+ [Pop T0] ++
        [TextLabel doneLabel, Push T0]
