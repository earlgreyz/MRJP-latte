module TAC.Instruction where

type Label = Integer
type Reg = Integer
type Fun = String

data Instruction
  = IQuad Reg QOp IArg IArg    -- x := y `op` z
  | ITrip Reg TOp IArg         -- x := op y
  | ICopy Reg IArg             -- x := y
  | IJmp Label                 -- goto L
  | ICond IArg COp IArg Label  -- if x `op` y goto L
  | IParam IArg                -- param x
  | ICall Reg Fun Integer      -- call f, n
  | IRet IArg                  -- return x
  | IPut Reg IArg IArg         -- x[i] := y
  | IGet Reg Reg IArg          -- x := y[i]

data IArg
  = IArgReg Reg
  | IArgVal Integer

data QOp = QOpAdd | QOpSub | QOpMul | QOpDiv | QOpAnd | QOpOr | QOpXor
data TOp = TOpNeg | TOpNot
data COp = CLt | CGt | CEq | CLe | CGe | CNe
