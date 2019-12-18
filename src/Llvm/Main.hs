module Llvm.Main (compileProgram) where

import qualified Data.Map as M

import Control.Monad.Identity
import Control.Monad.RWS.Lazy
import Control.Monad.State
import Control.Monad.Writer

import qualified Latte.AbsLatte as L

import Llvm.Compiler
import Llvm.Llvm
import Llvm.TopDefinition

runCompileProgram :: L.Program a -> Program
runCompileProgram p =
  let ((ds, _, fs), cs) = runConstantBuilder $ evalBlockBuilder $ evalFunctionBuilder $ runCompilerRWST $ compileProgram p in
    Program (ds, cs, fs)
  where
    runCompilerRWST m =
      runRWST m (M.empty, M.empty) (Register 0, Label 0)
    evalFunctionBuilder m =
      evalStateT m ((Tvoid, "", []), [])
    evalBlockBuilder m =
      evalStateT m (Label 0, [])
    runConstantBuilder m =
      runIdentity $ runWriterT $ evalStateT m 0

compileProgram :: L.Program a -> Compiler [Declaration]
compileProgram (L.Program _ ts) = mapM compileTopDef ts
