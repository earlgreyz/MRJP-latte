module Llvm.Compiler where

import qualified Data.Map as M

import Control.Monad.Writer
import Control.Monad.RWS.Lazy

import qualified Latte.AbsLatte as L

import Llvm.Instruction

type Variables = M.Map L.Ident (Type, Register)
type Functions = M.Map L.Ident (Type, Label)

type Environment = (Variables, Functions)
type State = (Register, Label, [Constant], Block)

type Compiler = RWST Environment [Block] State IO

emitInstruction :: Instruction -> Compiler ()
emitInstruction (ILabel l) = do
  (nr, nl, cs, block) <- get
  tell $ [block]
  modify $ \_ -> (nr, nl, cs, newBlock l)
emitInstruction i = do
  (nr, nl, cs, b) <- get
  modify $ \_ -> (nr, nl, cs, blockAppend b i)

-- Allocates new temporary register.
freshTemp :: Compiler Register
freshTemp = do
  (nr, nl, cs, block) <- get
  modify $ \_ -> (nextRegister nr, nl, cs, block)
  return nr

-- Allocates new label.
freshLabel :: Compiler Label
freshLabel = do
  (nr, nl, cs, block) <- get
  modify $ \_ -> (nr, nextLabel nl, cs, block)
  return nl

-- Allocates new constant.
newConstant :: String -> Compiler Constant
newConstant s = do
  (nr, nl, cs, block) <- get
  let c = Constant (toInteger $ length cs) s
  modify $ \_ -> (nr, nl, (c:cs), block)
  return c

-- Returns variables map.
askVariables :: Compiler Variables
askVariables = ask >>= \(vs, _) -> return vs

-- Returns functions map.
askFunctions :: Compiler Functions
askFunctions = ask >>= \(_, fs) -> return fs
