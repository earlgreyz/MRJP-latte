module TAC.Compiler where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.RWS.Lazy

import Latte.AbsLatte

import TAC.Instruction

-- Labels and Variables mapping
type Env = (M.Map Label Instruction, M.Map Ident Reg)
-- (Next register, Next label)
type State = (Reg, Label)

type IExcept = ExceptT String IO
type Compiler = RWST Env [Instruction] State IExcept
