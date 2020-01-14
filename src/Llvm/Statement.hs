{-# LANGUAGE ScopedTypeVariables #-}
module Llvm.Statement (compileStmt, compileBlock) where

import Data.Foldable
import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.State

import qualified Latte.AbsLatte as L
import Latte.PrintLatte

import Constexpr.Evaluate
import qualified Constexpr.Value as C

import Llvm.Compiler
import Llvm.Expression
import Llvm.Internal
import Llvm.Llvm
import Llvm.Util

compileBlock :: L.Block a -> Compiler ()
compileBlock (L.Block _ ss) = do
  label <- freshLabel
  emitInstruction $ IBr label
  emitInstruction $ ILabel label
  compileManyStmt ss

compileManyStmt :: [L.Stmt a] -> Compiler ()
compileManyStmt ss = ask >>= \env ->
  foldlM (\env s -> local (const env) $ compileStmt s) env ss >> return ()

compileDecl :: L.Type a -> L.Item a -> Compiler Env
compileDecl t (L.Init _ x e) = do
  (tv, v) <- compileExpr e
  reg <- freshRegister
  emitInstruction $ IAlloca tv reg
  emitInstruction $ IStore tv v reg
  (vs, fs, cs) <- ask
  return (M.insert x (tv, reg) vs, fs, cs)
compileDecl t (L.NoInit a x) = compileDecl t $ L.Init a x (defaultValue t) where
  defaultValue :: L.Type a -> L.Expr a
  defaultValue t = case t of
    L.Int a -> L.ELitInt a 0
    L.Bool a -> L.ELitFalse a
    L.Str a -> L.EString a "\"\""
    L.Array a t -> L.ENewArr a t (L.ELitInt a 0)
    L.Class a cls -> L.ENewObj a cls

compileStmt :: forall a. L.Stmt a -> Compiler Env
compileStmt (L.Empty _) = ask
compileStmt (L.BStmt _ b) = compileBlock b >> ask
compileStmt (L.Decl _ t ds) = ask >>= \env ->
  foldlM (\env d -> local (const env) $ compileDecl t d) env ds
compileStmt (L.Ass _ x e) = do
  (_, ptr) <- getValuePointer x
  (t, v) <- compileExpr e
  emitInstruction $ IStore t v ptr
  ask
compileStmt (L.Incr a x) = compileStmt $ L.Ass a x (
  L.EAdd a (L.EVar a x) (L.Plus a) (L.ELitInt a 1))
compileStmt (L.Decr a x) = compileStmt $ L.Ass a x (
  L.EAdd a (L.EVar a x) (L.Minus a) (L.ELitInt a 1))
compileStmt (L.Ret _ e) = do
  (t, v) <- compileExpr e
  emitInstruction $ IRet t v
  ask
compileStmt (L.VRet _) = do
  emitInstruction $ IRet Tvoid (VInt 0)
  ask
compileStmt (L.CondElse _ e st sf) = case tryEval e of
  Just (C.VBool True) -> compileStmt st
  Just (C.VBool False) -> compileStmt sf
  Nothing -> do
    (_, b) <- compileExpr e
    tlabel <- freshLabel
    flabel <- freshLabel
    retlabel <- freshLabel
    emitInstruction $ IComment $ printTree e
    emitInstruction $ IBrCond b tlabel flabel
    -- True block.
    emitInstruction $ ILabel tlabel
    compileStmt st
    emitInstruction $ IBr retlabel
    -- False block.
    emitInstruction $ ILabel flabel
    compileStmt sf
    emitInstruction $ IBr retlabel
    -- After conditional block.
    emitInstruction $ ILabel retlabel
    ask
compileStmt (L.Cond _ e s) = case tryEval e of
  Just (C.VBool True) -> compileStmt s
  Just (C.VBool False) -> ask
  Nothing -> do
    (_, b) <- compileExpr e
    tlabel <- freshLabel
    retlabel <- freshLabel
    emitInstruction $ IComment $ printTree e
    emitInstruction $ IBrCond b tlabel retlabel
    -- True block.
    emitInstruction $ ILabel tlabel
    compileStmt s
    emitInstruction $ IBr retlabel
    -- After conditional block.
    emitInstruction $ ILabel retlabel
    ask
compileStmt (L.While _ e s) = case tryEval e of
  Just (C.VBool True) -> do
    label <- freshLabel
    emitInstruction $ IBr label
    -- Body.
    emitInstruction $ ILabel label
    compileStmt s
    emitInstruction $ IBr label
    -- Continue.
    contlabel <- freshLabel
    emitInstruction $ ILabel contlabel
    ask
  Just (C.VBool False) -> ask
  Nothing -> do
    condlabel <- freshLabel
    bodylabel <- freshLabel
    contlabel <- freshLabel
    emitInstruction $ IBr condlabel
    -- Conditional.
    emitInstruction $ ILabel condlabel
    (_, b) <- compileExpr e
    emitInstruction $ IBrCond b bodylabel contlabel
    -- Body.
    emitInstruction $ ILabel bodylabel
    compileStmt s
    emitInstruction $ IBr condlabel
    -- Continue.
    emitInstruction $ ILabel contlabel
    ask
compileStmt (L.ForEach a t x array s) = compileBlock block >> ask
  where
    counterIdent :: L.Ident
    counterIdent = L.Ident "for"
    counterLValue :: L.LValue a
    counterLValue = L.LVar a counterIdent
    counterExpr :: L.Expr a
    counterExpr = L.EVar a counterLValue
    arrayLength :: L.Expr a
    arrayLength = L.EVar a (L.LAttr a array lengthIdent)
    block :: L.Block a
    block = L.Block a [
      L.Decl a (L.Int a) [L.Init a counterIdent (L.ELitInt a 0)],
      L.Decl a t [L.NoInit a x],
      L.While a (L.ERel a counterExpr (L.LTH a) arrayLength) (L.BStmt a $ L.Block a [
        L.Ass a (L.LVar a x) (L.EVar a $ L.LAt a array counterExpr),
        s,
        L.Incr a counterLValue])]
compileStmt (L.SExp _ e) = compileExpr e >> ask
