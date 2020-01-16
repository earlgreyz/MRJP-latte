{-# LANGUAGE BlockArguments #-}
module Llvm.TopDefinition (collectDeclaration, compileTopDef) where

import Data.Foldable
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Reader

import qualified Latte.AbsLatte as L

import Llvm.Compiler
import Llvm.Internal
import Llvm.Llvm
import Llvm.Statement
import Llvm.Util

-- Converts method into standalone function.
convertMethod :: L.Ident -> L.FunDef a -> L.FunDef a
convertMethod c@(L.Ident cls) (L.FunDef a t (L.Ident fident) args block) =
  let self = L.Arg a (L.Class a c) selfIdent in
    L.FunDef a t (L.Ident $ cls ++ "." ++ fident) (self:args) block

-- Extracts declaration parameters of a function.
functionDeclaration :: L.FunDef a -> (Type, L.Ident, String, [Type])
functionDeclaration (L.FunDef _ t fident args _) =
  let rt = convertType t in
    let ts = map (\(L.Arg _ t _) -> convertType t) args in
      (rt, fident, convertFunctionName fident, ts)

-- Collect attribute definitons from class fields list.
collectAttrs :: [L.Field a] -> Integer -> Attributes -> (Integer, Attributes)
collectAttrs [] off fs = (off, fs)
collectAttrs ((L.Attr _ t x):xs) off fs = let tt = convertType t in
  collectAttrs xs (off + typeSize tt) $ M.insert x (tt, off) fs
collectAttrs ((L.Method _ _):xs) off fs = collectAttrs xs off fs

collectMethods :: L.Ident -> [L.Field a] -> Methods -> Methods
collectMethods _ [] ms = ms
collectMethods cls ((L.Method _ f@(L.FunDef _ _ fident _ _)):xs) ms =
  let (rt, _, fname, _) = functionDeclaration $ convertMethod cls f in
    collectMethods cls xs $ M.insert fident (rt, fname) ms
collectMethods cls ((L.Attr _ _ _):xs) ms = collectMethods cls xs ms

-- Collect method definitons from class fields list.
collectMethodDeclaration :: L.Ident -> L.Field a -> Compiler Functions
collectMethodDeclaration cls (L.Method _ f) = do
  fs <- askFunctions
  let (rt, fident, fname, ts) = functionDeclaration $ convertMethod cls f
  return $ M.insert fident (rt, fname) fs
collectMethodDeclaration cls (L.Attr _ _ _) = askFunctions

-- Collect declarations from top definitions.
collectDeclaration :: L.TopDef a -> Compiler Env
collectDeclaration (L.FnDef _ f) = ask >>= \(vs, fs, cs) -> do
  let (rt, fident, fname, _) = functionDeclaration f
  return (vs, M.insert fident (rt, fname) fs, cs)
collectDeclaration (L.ClDef _ cls@(L.Ident c) fields) = ask >>= \(vs, fs, cs) -> do
  -- First bytes are used for storing the class name.
  let (size, as) = collectAttrs fields (typeSize $ Ptr Ti8) M.empty
  let ms = collectMethods cls fields M.empty
  fs' <- foldlM (\fs f -> localFunctions (const fs) $ collectMethodDeclaration cls f) fs fields
  name <- newConstant c
  return (vs, fs, M.insert cls (size, name, as, ms) cs)
collectDeclaration (L.ClExtDef _ cls@(L.Ident c) base fields) = ask >>= \(vs, fs, cs) -> do
  let (size, _, as, ms) = cs M.! base
  let (size', as') = collectAttrs fields size as
  let ms' = collectMethods cls fields ms
  fs' <- foldlM (\fs f -> localFunctions (const fs) $ collectMethodDeclaration cls f) fs fields
  name <- newConstant c
  return (vs, fs, M.insert cls (size', name, as', ms') cs)

compileFunction :: L.FunDef a -> Compiler ()
compileFunction f@(L.FunDef _ t _ args block) = do
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

compileMethod :: L.Ident -> L.Field a -> Compiler ()
compileMethod _ (L.Attr _ _ _) = return ()
compileMethod cls (L.Method _ f) = compileFunction $ convertMethod cls f

compileTopDef :: L.TopDef a -> Compiler ()
compileTopDef (L.FnDef _ f) = compileFunction f
compileTopDef (L.ClDef _ cls fields) = mapM_ (compileMethod cls) fields
compileTopDef (L.ClExtDef _ cls _ fields) = mapM_ (compileMethod cls) fields
