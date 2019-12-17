module Llvm.Statement (compileStmt, compileBlock) where

import Data.Foldable
import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.State

import qualified Latte.AbsLatte as L

import Llvm.Compiler
import Llvm.Expression
import Llvm.Llvm

compileBlock :: L.Block a -> Compiler ()
compileBlock (L.Block _ ss) = compileManyStmt ss

compileManyStmt :: [L.Stmt a] -> Compiler ()
compileManyStmt ss = ask >>= \env ->
  foldlM (\env s -> local (const env) $ compileStmt s) env ss >> return ()

compileDecl :: L.Type a -> L.Item a -> Compiler Env
compileDecl t (L.Init _ x e) = do
  (tv, v) <- compileExpr e
  reg <- freshRegister
  emitInstruction $ IAlloca tv reg
  emitInstruction $ IStore tv v reg
  (vs, fs) <- ask
  return (M.insert x (tv, reg) vs, fs)
compileDecl t (L.NoInit a x) = compileDecl t $ L.Init a x (defaultValue t) where
  defaultValue :: L.Type a -> L.Expr a
  defaultValue t = case t of
    L.Int a -> L.ELitInt a 0
    L.Bool a -> L.ELitFalse a
    L.Str a -> L.EString a ""

compileStmt :: L.Stmt a -> Compiler Env
compileStmt (L.Empty _) = ask
compileStmt (L.BStmt _ b) = compileBlock b >> ask
compileStmt (L.Decl _ t ds) = ask >>= \env ->
  foldlM (\env d -> local (const env) $ compileDecl t d) env ds
compileStmt (L.Ass _ x e) = do
  vs <- askVariables
  let (_, reg) = vs M.! x
  (t, v) <- compileExpr e
  emitInstruction $ IStore t v reg
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
compileStmt (L.CondElse _ e st sf) = error "Unimplemented"
compileStmt (L.Cond _ e s) = error "Unimplemented"
compileStmt (L.While _ e s) = error "Unimplemented"
compileStmt (L.SExp _ e) = compileExpr e >> ask
