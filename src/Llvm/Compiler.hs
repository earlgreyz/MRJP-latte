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

type Env = (Variables, Functions)
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

emitInstruction :: Instruction -> Compiler ()
emitInstruction (ILabel l) = do
  modifyBlockBuilder $ \_ -> (l, False, [])
emitInstruction i = getBlockFinalized >>= \f -> unless f $ doEmitInstruction i
  where
    doEmitInstruction :: Instruction -> Compiler ()
    doEmitInstruction i = do
      let final = isFinalInstruction i
      modifyBlockBuilder $ \(l, _, is) -> (l, final, is ++ [i])
      when (final) $ getBlockBuilder >>= \(l, _, is) -> emitBlock $ Block (l, is)

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
askVariables = ask >>= \(vs, _) -> return vs

localVariables :: (Variables -> Variables) -> Compiler a -> Compiler a
localVariables f compiler = local (\(vs, fs) -> (f vs, fs)) compiler

-- Returns functions map.
askFunctions :: Compiler Functions
askFunctions = ask >>= \(_, fs) -> return fs

localFunctions :: (Functions -> Functions) -> Compiler a -> Compiler a
localFunctions f compiler = local (\(vs, fs) -> (vs, f fs)) compiler
