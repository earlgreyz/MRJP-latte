module Llvm.Instruction where

import Evaluate.Value

newtype Register = Register Integer deriving (Eq, Ord)

nextRegister :: Register -> Register
nextRegister (Register r) = Register $ r + 1

newtype Label = Label Integer deriving (Eq, Ord)

nextLabel :: Label -> Label
nextLabel (Label r) = Label $ r + 1

-- Result can be a register or a variable name.
data Result = RRegister Register | RVariable String deriving (Eq)
-- LLVM types.
data Type = Ti64 | Ti32 | Tvoid | Ti1 | Ti8 | Ptr Type deriving (Eq, Ord)

-- Instructions used in LLVM.
data Instruction
  = ICall Type String [(Type, Value)] (Maybe Result)
  | IRet Type Value
  | IArithm Type Value Value ArithmOp Result
  | IBr Label
  | IBrCond Type Value Label Label
  | ILabel Label
  | ILoad Type Type Result Result
  | IStore Type Value Type Result
  | IAlloca Type Result
  | IIcmp Cond Type Value Value Result
  | IPhi Type [(Value, Label)] Result
  | IUnreachable
  | IGetElementPtr Type (Type, Value) (Type, Value) Result
  | IBitcast (Type, Value) Type Result
  | ISext (Type, Value) Type Result
  deriving Eq
-- Operations in LLVM.
data ArithmOp = OpAdd | OpSub | OpMul | OpSDiv | OpSRem deriving Eq
data Cond = CondEQ | CondNE | CondSGT | CondSGE | CondSLT | CondSLE deriving Eq

-- Block of instructions.
newtype Block = Block (Label, [Instruction]) deriving Eq

-- Function definition.
newtype Function = Function (Type, String, [(Type, String)], [Block]) deriving Eq

-- Helper functions.
newBlock :: Label -> Block
newBlock l = Block (l, [])

blockAppend :: Block -> Instruction -> Block
blockAppend (Block (l, is)) i = Block (l, is ++ [i])
