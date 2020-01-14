{-# LANGUAGE BlockArguments #-}
module Llvm.TopDefinition (collectDeclaration, compileTopDef) where

import Control.Monad
import qualified Data.Map as M

import qualified Latte.AbsLatte as L

import Llvm.Compiler
import Llvm.Llvm
import Llvm.Statement
import Llvm.Util

functionDeclaration :: L.FunDef a -> (Type, L.Ident, String, [Type])
functionDeclaration (L.FunDef _ t fident args _) =
  let rt = convertType t in
    let ts = map (\(L.Arg _ t _) -> convertType t) args in
      (rt, fident, convertFunctionName fident, ts)

collectDeclaration :: L.TopDef a -> Compiler Declaration
collectDeclaration (L.FnDef _ f) = do
  let (rt, fident, fname, ts) = functionDeclaration f
  return $ DeclFun rt fident fname ts
collectDeclaration (L.ClDef _ cls fields) = do
  let (size, fs) = collectAttrs fields 0 M.empty
  return $ DeclClass cls size fs
  where
    collectAttrs :: [L.Field a] -> Integer -> Fields -> (Integer, Fields)
    collectAttrs [] off fs = (off, fs)
    collectAttrs ((L.Attr _ t x):xs) off fs = let tt = convertType t in
      collectAttrs xs (off + typeSize tt) $ M.insert x (tt, off) fs
    collectAttrs ((L.Method _ _):xs) off fs = collectAttrs xs off fs

compileTopDef :: L.TopDef a -> Compiler ()
compileTopDef (L.FnDef _ f@(L.FunDef _ t _ args block)) = do
  let (rt, _, fname, ts) = functionDeclaration f
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
      Ptr Ti8 -> emitInstruction $ IRet (Ptr Ti8) (VInt 0)
      Array t -> emitInstruction $ IRet (Array t) VNull
      Object cls -> emitInstruction $ IRet (Object cls) VNull
compileTopDef (L.ClDef _ cls fields) = return ()
