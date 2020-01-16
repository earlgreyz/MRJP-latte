module Llvm.Main (runCompileProgram) where

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe

import Control.Monad.Identity
import Control.Monad.RWS.Lazy
import Control.Monad.State
import Control.Monad.Writer

import qualified Latte.AbsLatte as L

import Llvm.Compiler
import Llvm.Internal
import Llvm.Llvm
import Llvm.TopDefinition

fromDeclarations :: [Declaration] -> Functions
fromDeclarations ds = M.fromList $ map convert ds
  where
    convert :: Declaration -> (L.Ident, (Type, String))
    convert (Declaration rt ident fname _) = (ident, (rt, fname))

runCompileProgram :: L.Program a -> Program
runCompileProgram p =
  let ((_, fs), cs) = runConstantBuilder $ evalBlockBuilder $ evalFunctionBuilder $ execCompilerRWST $ compileProgram p in
    Program (internalFunctions, cs, fs)
  where
    execCompilerRWST m =
      execRWST m (M.empty, fromDeclarations $ internalFunctions, M.empty) (Register 0, Label 0)
    evalFunctionBuilder m =
      evalStateT m ((Tvoid, "", []), [])
    evalBlockBuilder m =
      evalStateT m (Label 0, True, [])
    runConstantBuilder m =
      runIdentity $ runWriterT $ evalStateT m 0

compileProgram :: L.Program a -> Compiler ()
compileProgram (L.Program _ ts) = ask >>= \env -> do
  env <- foldlM (\env t -> local (const env) $ collectDeclaration t) env ts
  local (const env) $ mapM_ compileTopDef ts
