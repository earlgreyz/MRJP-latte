module Llvm.Main (runCompileProgram) where

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
fromDeclarations ds = M.fromList $ mapMaybe convert ds
  where
    convert :: Declaration -> Maybe (L.Ident, (Type, String))
    convert (DeclFun rt ident fname _) = Just (ident, (rt, fname))
    convert (DeclClass _ _ _) = Nothing

classesFromDeclarations :: [Declaration] -> Classes
classesFromDeclarations ds = M.fromList $ mapMaybe convert ds
  where
    convert :: Declaration -> Maybe (L.Ident, (Integer, Fields))
    convert (DeclClass cls size fs) = Just (cls, (size, fs))
    convert (DeclFun _ _ _ _) = Nothing

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
compileProgram (L.Program _ ts) = do
  ds <- mapM collectDeclaration ts
  let cs = classesFromDeclarations $ concat ds
  let fs = fromDeclarations $ concat ds
  localClasses (const cs) $ localFunctions (M.union fs) $ mapM_ compileTopDef ts
