module TAC.Compiler where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.RWS.Lazy

import Latte.AbsLatte

import TAC.Instruction

type Regs = M.Map Ident Reg
type Labels = M.Map Label Instruction

-- Labels and Variables mapping
type Env = (Regs, Labels)
-- (Next register, Next label)
type State = (Reg, Label)

type IExcept = ExceptT String IO
type Compiler = RWST Env [Instruction] State IExcept

-- Helper functions
freshTemp :: Compiler Reg
freshTemp = do
  (r, l) <- get
  modify $ \_ -> (r + 1, l)
  return r

freshLabel :: Compiler Label
freshLabel = do
  (r, l) <- get
  modify $ \_ -> (r, l + 1)
  return l
