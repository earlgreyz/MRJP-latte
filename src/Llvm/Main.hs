module Llvm.Main (runCompileProgram) where

import qualified Data.Map as M

import Control.Monad.Identity
import Control.Monad.RWS.Lazy
import Control.Monad.State
import Control.Monad.Writer

import qualified Latte.AbsLatte as L

import Llvm.Compiler
import Llvm.Llvm
import Llvm.TopDefinition

internalFunctions :: [Declaration]
internalFunctions = [
  DeclFun Tvoid (L.Ident "printInt") "printInt" [Ti32],
  DeclFun Tvoid (L.Ident "printString") "printString" [Ptr Ti8],
  DeclFun Tvoid (L.Ident "error") "error" [],
  DeclFun Ti32 (L.Ident "readInt") "readInt" [],
  DeclFun (Ptr Ti8) (L.Ident "readString") "readString" []]

fromDeclarations :: [Declaration] -> Functions
fromDeclarations ds = M.fromList $ map convert ds
  where
    convert :: Declaration -> (L.Ident, (Type, String))
    convert (DeclFun rt ident fname _) = (ident, (rt, fname))

runCompileProgram :: L.Program a -> Program
runCompileProgram p =
  let ((ds, _, fs), cs) = runConstantBuilder $ evalBlockBuilder $ evalFunctionBuilder $ runCompilerRWST $ compileProgram p in
    Program (internalFunctions ++ ds, cs, fs)
  where
    runCompilerRWST m =
      runRWST m (M.empty, fromDeclarations $ internalFunctions) (Register 0, Label 0)
    evalFunctionBuilder m =
      evalStateT m ((Tvoid, "", []), [])
    evalBlockBuilder m =
      evalStateT m (Label 0, [])
    runConstantBuilder m =
      runIdentity $ runWriterT $ evalStateT m 0

compileProgram :: L.Program a -> Compiler [Declaration]
compileProgram (L.Program _ ts) = do
  ds <- mapM collectDeclaration ts
  localFunctions (\fs -> M.union fs $ fromDeclarations ds) $ mapM_ compileTopDef ts
  return ds
