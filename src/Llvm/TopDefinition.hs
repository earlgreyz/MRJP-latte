{-# LANGUAGE BlockArguments #-}
module Llvm.TopDefinition (collectDeclaration, compileTopDef) where

import Control.Monad
import qualified Data.Map as M

import qualified Latte.AbsLatte as L

import Llvm.Compiler
import Llvm.Llvm
import Llvm.Statement
import Llvm.Util

collectDeclaration :: L.TopDef a -> Compiler Declaration
collectDeclaration (L.FnDef _ t fname args _) = do
  let rt = convertType t
  let ts = map (\(L.Arg _ t _) -> convertType t) args
  return $ DeclFun rt fname (convertFunctionName fname) ts

compileTopDef :: L.TopDef a -> Compiler ()
compileTopDef f@(L.FnDef _ t _ args block) = do
  (DeclFun rt _ fname ts) <- collectDeclaration f
  -- Collect argument names.
  let xs = map (\(L.Arg _ _ x) -> x) args
  -- Generate registers for paramaters.
  params <- mapM (\_ -> freshRegister) args
  -- Start function blocks.
  startFunction rt fname $ zip ts params
  -- Initialize argument variabbles.
  argregs <- mapM initArgument $ zip ts params
  -- Create parameters variables map.
  let vars = M.fromList $ zip xs $ zip ts argregs
  -- Execute block with local variables
  localVariables (\vs -> M.union vs $ vars) $ compileBlock block
  -- Implicitly return default value so all blocks get generated.
  returnDefault rt
  endFunction
  where
    initArgument :: (Type, Register) -> Compiler Register
    initArgument (t, v) = do
      reg <- freshRegister
      emitInstruction $ IAlloca t reg
      emitInstruction $ IStore t (VReg v) reg
      return reg
    returnDefault :: Type -> Compiler ()
    returnDefault t = case t of
      Tvoid -> emitInstruction $ IRet Tvoid (VInt 0)
      Ti1 -> emitInstruction $ IRet Ti1 (VBool False)
      Ti32 -> emitInstruction $ IRet Ti32 (VInt 0)
      Ptr Ti8 -> newConstant "" >>= \c -> emitInstruction $ IRet (Ptr Ti8) (VConst c)
      Array t -> newConstant "\0\0\0\0" >>= \c -> emitInstruction $ IRet (Array t) (VConst c)
      _ -> error "unknown type"
