{-# LANGUAGE BlockArguments #-}
module Llvm.TopDefinition where

import qualified Data.Map as M

import qualified Latte.AbsLatte as L

import Llvm.Compiler
import Llvm.Llvm
import Llvm.Statement
import Llvm.Util

compileTopDef :: (L.TopDef a) -> Compiler ()
compileTopDef (L.FnDef _ t f args block) = do
  let rt = convertType t
  -- Collect argument types.
  let ts = map (\(L.Arg _ t _) -> convertType t) args
  -- Collect argument names.
  let xs = map (\(L.Arg _ _ x) -> x) args
  -- Generate registers for paramaters.
  params <- mapM (\_ -> freshRegister) args
  -- Start function blocks.
  startFunction rt (show f) $ zip ts params
  -- Initialize argument variabbles.
  argregs <- mapM initArgument $ zip ts params
  -- Create parameters variables map.
  let vars = M.fromList $ zip xs $ zip ts argregs
  -- Execute block with local variables
  localVariables (\vs -> M.union vs $ vars) $ compileBlock block
  endFunction
  where
    initArgument :: (Type, Register) -> Compiler Register
    initArgument (t, v) = do
      reg <- freshRegister
      emitInstruction $ IAlloca t reg
      emitInstruction $ IStore t (VReg v) reg
      return reg
