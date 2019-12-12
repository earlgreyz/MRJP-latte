module Llvm.Instruction where

import qualified Latte.AbsLatte as L

newtype Register = Register Integer deriving (Eq, Ord)
newtype Label = Label Integer deriving (Eq, Ord)
data Constant = Constant Integer String deriving (Eq, Ord)

-- LLVM types.
data Type = Ti64 | Ti32 | Tvoid | Ti1 | Ti8 | Ptr Type deriving (Eq, Ord)
-- LLVM values.
data Value = VInt Integer | VReg Register | VBool Bool | VConst Constant deriving (Eq, Ord)

-- Instructions used in LLVM.
data Instruction
  = ICall Type String [(Type, Value)] (Maybe Register)
  | IRet Type Value
  | IArithm ArithmOp Value Value Register
  | IBr Label
  | IBrCond Type Value Label Label
  | ILabel Label
  | ILoad Type Register Register
  | IStore Type Value Type Register
  | IAlloca Type Register
  | IIcmp Cond Type Value Value Register
  | IPhi Type [(Value, Label)] Register
  | IUnreachable
  | IGetElementPtr Type (Type, Value) (Type, Value) Register
  | IBitcast (Type, Value) Type Register
  | ISext (Type, Value) Type Register
  deriving Eq
-- Operations in LLVM.
data ArithmOp = OpAdd | OpSub | OpMul | OpDiv | OpMod deriving Eq
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

nextRegister :: Register -> Register
nextRegister (Register r) = Register $ r + 1

nextLabel :: Label -> Label
nextLabel (Label l) = Label $ l + 1

getConstantIdent :: Constant -> Integer
getConstantIdent (Constant c _) = c

isConstant :: Value -> Bool
isConstant (VConst _) = True
isConstant _ = False

convertType :: L.Type a -> Type
convertType t = case t of
  L.Void _ -> Tvoid
  L.Int _ -> Ti32
  L.Bool _ -> Ti1
  L.Str _ -> Ptr Ti8
