module Llvm.Llvm where

import qualified Latte.AbsLatte as L

import Data.List

-- Register names.
newtype Register = Register Integer deriving (Eq, Ord)
instance Show Register where
  show (Register r) = "%r_" ++ (show r)

nextRegister :: Register -> Register
nextRegister (Register r) = Register $ r + 1

-- Jump labels.
newtype Label = Label Integer deriving (Eq, Ord)
instance Show Label where
  show (Label i) = "L_" ++ (show i)

nextLabel :: Label -> Label
nextLabel (Label l) = Label $ l + 1

-- String constants.
data Constant = Constant Integer String deriving (Eq, Ord)
instance Show Constant where
  show (Constant i s) = intercalate "" [
    "@const_", show i, " = private unnamed_addr constant [",
    show (1 + length s), " x i8] c\"", escape s, "\\00\", align 1" ]
    where
      escape :: String -> String
      escape [] = []
      escape ('\t':s) = '\\':'0':'9':escape s
      escape ('\n':s) = '\\':'0':'a':escape s
      escape ('\"':s) = '\\':'2':'2':escape s
      escape ('\\':s) = '\\':'5':'c':escape s
      escape (a:s) = a:escape s

-- LLVM types.
data Type = Ti64 | Ti32 | Ti8 | Ti1 | Tvoid | Ptr Type deriving (Eq, Ord)
instance Show Type where
  show Ti64 = "i64"
  show Ti32 = "i32"
  show Ti8 = "i8"
  show Ti1 = "i1"
  show Tvoid = "void"
  show (Ptr t) = (show t) ++ "*"

-- LLVM values.
data Value = VInt Integer | VReg Register | VBool Bool | VConst Constant deriving (Eq, Ord)
instance Show Value where
  show (VInt i) = show i
  show (VReg r) = show r
  show (VBool b) = show $ fromEnum b
  show (VConst (Constant i s)) = intercalate "" [
    "getelementptr inbounds ([", len , " x i8], [", len, " x i8]* @const_",
    show i, ", i32 0, i32 0)"] where len = show (1 + length s)

-- Instructions used in LLVM.
data Instruction
  = ICall Type String [(Type, Value)] (Maybe Register)
  | IRet Type Value
  | IArithm ArithmOp Value Value Register
  | IBr Label
  | IBrCond Value Label Label
  | ILabel Label
  | ILoad Type Register Register
  | IStore Type Value Register
  | IAlloca Type Register
  | IIcmp Cond Type Value Value Register
  | IPhi Type [(Value, Label)] Register
  | IComment String
  deriving Eq
instance Show Instruction where
  show (ICall rt f args res) = intercalate "" [
    showRes res, "call ", show rt, " @", f, " (",
    intercalate ", " $ map showArg args, ")"]
    where
      showRes :: Maybe Register -> String
      showRes Nothing = ""
      showRes (Just reg) = show reg ++ " = "
      showArg :: (Type, Value) -> String
      showArg (t, v) = (show t) ++ " " ++ (show v)
  show (IRet Tvoid _) = "ret void"
  show (IRet t v) = "ret " ++ show t ++ " " ++ show v
  show (IArithm op v w res) = intercalate " " [
    show res, "=", show op, show Ti32, show v ++ ",", show w]
  show (IBr l) = "br label %" ++ show l
  show (IBrCond v l1 l2) =
    "br i1 " ++ show v ++ ", label %" ++ show l1 ++ ", label %" ++ show l2
  show (ILoad t addr res) =
    show res ++ " = load " ++ show t ++ ", " ++ show (Ptr t) ++ " " ++ show addr
  show (IStore t v res) = intercalate " " [
    "store", show t, show v ++ ",", show (Ptr t), show res]
  show (IAlloca t res) = show res ++ " = alloca " ++ show t
  show (IIcmp cond t v w res) = intercalate " " [
    show res, "= icmp", show cond, show t, show v ++ ",", show w]
  show (IPhi t args res) =
    show res ++ " = phi " ++ (show t) ++ (intercalate "," $ map showArg args)
    where
      showArg :: (Value, Label) -> String
      showArg (v, l) = "[" ++ show v ++ ", %" ++ show l ++ "]"
  show (IComment s) = "; " ++ s

-- Arithmetic operations.
data ArithmOp = OpAdd | OpSub | OpMul | OpDiv | OpMod deriving Eq
instance Show ArithmOp where
  show OpAdd = "add"
  show OpSub = "sub"
  show OpMul = "mul"
  show OpDiv = "sdiv"
  show OpMod = "srem"

-- Conditional operations.
data Cond = CondEQ | CondNE | CondSLE | CondSLT | CondSGE | CondSGT deriving Eq
instance Show Cond where
  show CondEQ = "eq"
  show CondNE = "ne"
  show CondSLE = "sle"
  show CondSLT = "slt"
  show CondSGE = "sge"
  show CondSGT = "sgt"

-- Block of instructions.
newtype Block = Block (Label, [Instruction], Instruction) deriving Eq
instance Show Block where
  show (Block (l, is, i)) = show l ++ ":\n" ++ (unlines $ map (indent . show) $ is ++ [i])
    where
      indent :: String -> String
      indent s = "  " ++ s

blockLabel :: Block -> Label
blockLabel (Block (l, _, _)) = l

-- Function definition.
newtype Function = Function (Type, String, [(Type, Register)], [Block]) deriving Eq
instance Show Function where
  show (Function (r, f, args, bs)) =
    "define " ++ show r ++ " @" ++ f ++
    "(" ++ (intercalate ", " $ map showArg args) ++ ") {\n" ++
    intercalate "\n" (map show bs) ++ "}"
    where
      showArg :: (Type, Register) -> String
      showArg (t, r) = (show t) ++ " " ++ (show r)

-- Top definition declaration.
data Declaration = DeclFun Type L.Ident String [Type] deriving Eq
instance Show Declaration where
  show (DeclFun r _ f args) =
    "declare " ++ show r ++ " @" ++ f ++
    "(" ++ (intercalate "," $ map show args) ++ ")"

-- Program.
newtype Program = Program ([Declaration], [Constant], [Function]) deriving Eq
instance Show Program where
  show (Program (ds, cs, fs)) =
    (unlines $ map show ds) ++ "\n" ++
    (unlines $ map show cs) ++ "\n" ++
    (unlines $ map show fs)
