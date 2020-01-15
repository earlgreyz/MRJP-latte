module Analyzer.Statement (analyzeStmt, analyzeBlock, startBlock, insertRet) where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable

import Latte.AbsLatte
import Latte.ErrLatte
import Latte.PrintLatte

import Analyzer.Analyzer
import Analyzer.Assert
import Analyzer.Error
import Analyzer.Expression
import Analyzer.Util

import Constexpr.Evaluate
import Constexpr.Value

-- Change _declared in current block_ to false.
startBlock :: Vars -> Vars
startBlock vs = M.map (\(_, t) -> (False, t)) vs

analyzeBlock :: (Block ErrPos) -> Analyzer ()
analyzeBlock (Block _ ss) =
  localVars (\vs -> startBlock vs) $ analyzeManyStmt ss

analyzeManyStmt :: [Stmt ErrPos] -> Analyzer ()
analyzeManyStmt ss = do
  startVars <- askVars
  foldlM (\vs s -> localVars (const vs) $ analyzeStmt s) startVars ss
  return ()

declare :: Type ErrPos -> Item ErrPos -> Analyzer Vars
declare t (NoInit a x) = do
  vs <- askVars
  assertNotRedeclared a x
  assertClassNotRedeclared a x
  return $ M.insert x (True, t) vs
declare t (Init a x e) = do
  vs <- askVars
  assertNotRedeclared a x
  assertClassNotRedeclared a x
  tt <- analyzeExpr e
  assertCanAssignType a (LVar a x) t tt
  return $ M.insert x (True, t) vs

-- Special marker for the return environment.
returnIdent :: Ident
returnIdent = Ident "return"

-- Insert function return type into the environment.
insertRet :: (Type ErrPos) -> Vars -> Vars
insertRet t env = M.insert returnIdent (True, t) env

analyzeReturn :: ErrPos -> Type ErrPos -> Analyzer Vars
analyzeReturn a tt = do
  t <- mustLookup a returnIdent
  unless (t == tt) $ throwError $ typeExpectedError a t tt
  modify $ \_ -> True
  askVars

analyzeCond :: Stmt ErrPos -> Analyzer Bool
analyzeCond s = do
  modify $ \_ -> False
  analyzeStmt s
  sret <- get
  return sret

analyzeStmt :: Stmt ErrPos -> Analyzer Vars
analyzeStmt (Empty _) = askVars
analyzeStmt (BStmt _ b) = analyzeBlock b >> askVars
analyzeStmt (Decl _ t []) = askVars
analyzeStmt (Decl a t (x:xs)) = do
  when (t == Void Nothing) $ throwError $ voidDeclarationError a
  vs <- declare t x
  localVars (const vs) $ analyzeStmt (Decl Nothing t xs)
analyzeStmt (Ass a x e) = do
  t <- analyzeLValue x
  tt <- analyzeExpr e
  assertCanAssignType a x t tt
  askVars
analyzeStmt (Incr a x) = do
  t <- analyzeLValue x
  unless (t == (Int Nothing)) $ throwError $ intExpectedError a x t
  askVars
analyzeStmt (Decr a x) = analyzeStmt $ Incr a x
analyzeStmt (Ret a e) = do
  tt <- analyzeExpr e
  analyzeReturn a tt
analyzeStmt (VRet a) = analyzeReturn a (Void Nothing)
analyzeStmt (CondElse _ e st sf) = do
  ret <- get -- Return already called.
  case tryEval e of
    Just (VBool True) -> do
      tret <- analyzeCond st
      modify $ \_ -> ret || tret
    Just (VBool False) -> do
      fret <- analyzeCond sf
      modify $ \_ -> ret || fret
    _ -> do
      analyzeExpr e
      tret <- analyzeCond st
      fret <- analyzeCond sf
      modify $ \_ -> ret || (tret && fret)
  askVars
analyzeStmt (Cond a e s) = analyzeStmt (CondElse a e s (Empty Nothing))
analyzeStmt (While a e s) = analyzeStmt (Cond a e s)
analyzeStmt (ForEach a t x array s) = do
  at <- analyzeExpr array
  case at of
    Array _ tt -> assertType a tt t
    otherwise -> throwError $ arrayError a array at
  localVars (\vs -> M.insert x (True, t) vs) $ analyzeStmt s
  askVars
analyzeStmt (SExp a e) = analyzeExpr e >> askVars
