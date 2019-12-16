module Llvm.Compiler where

import qualified Data.Map as M

import Control.Monad.RWS.Lazy
import Control.Monad.State
import Control.Monad.Writer

import qualified Latte.AbsLatte as L

import Llvm.Llvm

type Variables = M.Map L.Ident (Type, Register)
type Functions = M.Map L.Ident (Type, Label)

type Env = (Variables, Functions)
type CompilerState = (Register, Label)
type FunctionHeader = (Type, String, [(Type, Register)])

-- Monad used to produce constants.
type ConstantBuilderT a = StateT Integer (WriterT [Constant] a)
-- Monad used to build blocks from instructions.
type BlockBuilderState = (Label, [Instruction])
type BlockBuilderT a = StateT BlockBuilderState a
-- Monad used to build functions from blocks.
type FunctionBuilderState = (FunctionHeader, [Block])
type FunctionBuilderT a = StateT FunctionBuilderState (BlockBuilderT a)
-- All monads combined.
type Compiler = RWST Env [Function] CompilerState (FunctionBuilderT (ConstantBuilderT IO))

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
startFunction t f args = modifyFunctionBuilder $ \_ -> ((t, f, args), [])

endFunction :: Compiler ()
endFunction = do
  ((r, f, args), bs) <- getFunctionBuilder
  tell [Function (r, f, args, bs)]

emitBlock :: Block -> Compiler ()
emitBlock b = modifyFunctionBuilder $ \(h, bs) -> (h, bs ++ [b])

-- BlockBuilderT helper functions.
startBlock :: Label -> Compiler ()
startBlock l = modifyBlockBuilder $ \_ -> (l, [])

endBlock :: Compiler ()
endBlock = do
  (l, is) <- getBlockBuilder
  emitBlock $ Block (l, is)

emitInstruction :: Instruction -> Compiler ()
emitInstruction (ILabel l) = startBlock l
emitInstruction i = do
  modifyBlockBuilder $ \(l, is) -> (l, is ++ [i])
  when (isBranch i) $ endBlock

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

-- Returns functions map.
askFunctions :: Compiler Functions
askFunctions = ask >>= \(_, fs) -> return fs
