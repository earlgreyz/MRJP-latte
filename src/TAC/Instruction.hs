module TAC.Instruction where

type Label = Integer
type Reg = Integer
type Fun = String

data Instruction
  = IQuad Reg QOp IArg IArg    -- x := y `op` z
  | ITrip Reg TOp IArg         -- x := op y
  | ICopy Reg IArg             -- x := y
  | IJmp Label                 -- goto L
  | ICond COp IArg IArg Label  -- if x `op` y goto L
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

instance Show Instruction where
  show (IQuad x op y z) = "t" ++ (show x) ++ " := " ++ (show y) ++ " " ++ (show op) ++ " " ++ (show z)
  show (ITrip x op y) = "t" ++ (show x) ++ " := " ++ (show op) ++ " " ++ (show y)
  show (ICopy x y) = "t" ++ (show x) ++ " := " ++ (show y)
  show (IJmp l) = "goto L" ++ (show l)
  show (ICond op x y l) = "if " ++ (show x) ++ " " ++ (show op) ++ " " ++ (show y) ++ " goto L" ++ (show l)
  show (IParam x) = "param " ++ (show x)
  show (ICall x f n) = "t" ++ (show x) ++ " := call " ++ f ++ ", " ++ (show n)
  show (IRet x) = "return " ++ (show x)
  show (IPut x i y) = "t" ++ (show x) ++ "[" ++ (show i) ++ "] := " ++ (show y)
  show (IGet x y i) = "t" ++ (show x) ++ " := " ++ (show y) ++ "[" ++ (show i) ++ "]"

instance Show IArg where
  show (IArgReg x) = "t" ++ (show x)
  show (IArgVal n) = (show n)

instance Show QOp where
  show QOpAdd = "+"
  show QOpSub = "-"
  show QOpMul = "*"
  show QOpDiv = "/"
  show QOpAnd = "and"
  show QOpOr = "or"
  show QOpXor = "xor"

instance Show TOp where
  show TOpNeg = "-"
  show TOpNot = "not"

instance Show COp where
  show CLt = "<"
  show CGt = ">"
  show CEq = "=="
  show CLe = "<="
  show CGe = ">="
  show CNe = "!="
