module Llvm.Compiler where

import qualified Data.Map as M

import Control.Monad.Writer
import Control.Monad.RWS.Lazy

import qualified Latte.AbsLatte as L

import Llvm.Instruction

type Variables = M.Map L.Ident Register
type Functions = M.Map L.Ident Label

type Environment = (Variables, Functions)
type State = (Register, Label, Block)

type Compiler = RWST Environment [Block] State IO

emit :: Instruction -> Compiler ()
emit (ILabel l) = do
  (nr, nl, block) <- get
  tell $ [block]
  modify $ \_ -> (nr, nl, newBlock l)
emit i = do
  (nr, nl, b) <- get
  modify $ \_ -> (nr, nl, blockAppend b i)

-- Allocates new temporary register.
freshTemp :: Compiler Register
freshTemp = do
  (nr, nl, block) <- get
  modify $ \_ -> (nextRegister nr, nl, block)
  return nr

-- Allocates new label.
freshLabel :: Compiler Label
freshLabel = do
  (nr, nl, block) <- get
  modify $ \_ -> (nr, nextLabel nl, block)
  return nl

-- Returns variables map.
askVariables :: Compiler Variables
askVariables = ask >>= \(vs, _) -> return vs

-- Returns functions map.
askFunctions :: Compiler Functions
askFunctions = ask >>= \(_, fs) -> return fs
