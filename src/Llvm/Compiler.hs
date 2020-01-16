module Llvm.Compiler where

import qualified Data.Map as M

import Control.Monad.Identity
import Control.Monad.RWS.Lazy
import Control.Monad.State
import Control.Monad.Writer

import qualified Latte.AbsLatte as L

import Llvm.Llvm
import Llvm.Util

type Variables = M.Map L.Ident (Type, Register)
type Functions = M.Map L.Ident (Type, String)
type Classes = M.Map L.Ident Class

type Env = (Variables, Functions, Classes)
type CompilerState = (Register, Label)
type FunctionHeader = (Type, String, [(Type, Register)])

-- Monad used to produce constants.
type ConstantBuilderT a = StateT Integer (WriterT [Constant] a)
-- Monad used to build blocks from instructions.
type BlockBuilderState = (Label, Bool, [Instruction])
type BlockBuilderT a = StateT BlockBuilderState a
-- Monad used to build functions from blocks.
type FunctionBuilderState = (FunctionHeader, [Block])
type FunctionBuilderT a = StateT FunctionBuilderState (BlockBuilderT a)
-- All monads combined.
type CompilerConstantBuilder = ConstantBuilderT Identity
type CompilerFunctionBuilder = FunctionBuilderT CompilerConstantBuilder
type Compiler = RWST Env [Function] CompilerState CompilerFunctionBuilder

-- FunctionBuilderT lifted functions.
getFunctionBuilder :: Compiler FunctionBuilderState
getFunctionBuilder = lift get

modifyFunctionBuilder :: (FunctionBuilderState -> FunctionBuilderState) -> Compiler ()
modifyFunctionBuilder f = lift $ modify f

-- BlockBuilderT lifted functions.
getBlockBuilder :: Compiler BlockBuilderState
getBlockBuilder = lift $ lift get

modifyBlockBuilder :: (BlockBuilderState -> BlockBuilderState) -> Compiler ()
modifyBlockBuilder f = lift $ lift $ modify f

-- ConstantBuilderT lifted functions.
getConstantBuilder :: Compiler Integer
getConstantBuilder = lift $ lift $ lift get

modifyConstantBuilder :: (Integer -> Integer) -> Compiler ()
modifyConstantBuilder f = lift $ lift $ lift $ modify f

tellConstantBuilder :: [Constant] -> Compiler ()
tellConstantBuilder c = lift $ lift $ lift $ tell c

-- FunctionBuilderT helper functions.
startFunction :: Type -> String -> [(Type, Register)] -> Compiler ()
startFunction t f args = do
  modifyFunctionBuilder $ \_ -> ((t, f, args), [])
  label <- freshLabel
  emitInstruction $ ILabel label

endFunction :: Compiler ()
endFunction = do
  ((r, f, args), bs) <- getFunctionBuilder
  tell [Function (r, f, args, bs)]

emitBlock :: Block -> Compiler ()
emitBlock b = modifyFunctionBuilder $ \(h, bs) -> (h, bs ++ [b])

-- BlockBuilderT helper functions.
getBlockLabel :: Compiler Label
getBlockLabel = do
  (label, _, _) <- getBlockBuilder
  return label

getBlockFinalized :: Compiler Bool
getBlockFinalized = do
  (_, finalized, _) <- getBlockBuilder
  return finalized

setBlockFinalized :: Bool -> Compiler ()
setBlockFinalized f = do
  (l, _, is) <- getBlockBuilder
  modifyBlockBuilder $ \_ -> (l, f, is)

emitInstruction :: Instruction -> Compiler ()
emitInstruction (ILabel l) = do
  modifyBlockBuilder $ \_ -> (l, False, [])
emitInstruction i = getBlockFinalized >>= \f -> unless f $ doEmitInstruction i
  where
    doEmitInstruction :: Instruction -> Compiler ()
    doEmitInstruction i = do
      setBlockFinalized (isFinalInstruction i)
      (l, finalized, is) <- getBlockBuilder
      if finalized then
        emitBlock $ Block (l, is, i)
      else
        modifyBlockBuilder $ \(l, f, is) -> (l, f, is ++ [i])

-- ConstantBuilderT helper functions.
newConstant :: String -> Compiler Constant
newConstant s = do
  n <- getConstantBuilder
  modifyConstantBuilder $ \n -> n + 1
  let c = Constant n s
  tellConstantBuilder $ [c]
  return c

-- Allocates new register.
freshRegister :: Compiler Register
freshRegister = do
  (nr, nl) <- get
  modify $ \_ -> (nextRegister nr, nl)
  return nr

-- Allocates new label.
freshLabel :: Compiler Label
freshLabel = do
  (nr, nl) <- get
  modify $ \_ -> (nr, nextLabel nl)
  return nl

-- Returns variables map.
askVariables :: Compiler Variables
askVariables = ask >>= \(vs, _, _) -> return vs

localVariables :: (Variables -> Variables) -> Compiler a -> Compiler a
localVariables f compiler = local (\(vs, fs, cs) -> (f vs, fs, cs)) compiler

-- Returns functions map.
askFunctions :: Compiler Functions
askFunctions = ask >>= \(_, fs, _) -> return fs

localFunctions :: (Functions -> Functions) -> Compiler a -> Compiler a
localFunctions f compiler = local (\(vs, fs, cs) -> (vs, f fs, cs)) compiler

-- Returns classes map.
askClasses :: Compiler Classes
askClasses = ask >>= \(_, _, cs) -> return cs

localClasses :: (Classes -> Classes) -> Compiler a -> Compiler a
localClasses f compiler = local (\(vs, fs, cs) -> (vs, fs, f cs)) compiler
