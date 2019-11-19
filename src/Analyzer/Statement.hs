module Analyzer.Statement where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.Assert
import Analyzer.Error
import Analyzer.Expression
import Analyzer.Util

-- Insert arguments into the environment.
insertArgs :: [Arg ErrPos] -> Env -> Env
insertArgs args env = foldr insertArg env args where
  insertArg (Arg _ t arg) env = M.insert arg (True, t) env

-- Special marker for the return environment.
returnIdent :: Ident
returnIdent = Ident "return"

-- Insert function return type into the environment.
insertRet :: (Type ErrPos) -> Env -> Env
insertRet t env = M.insert returnIdent (True, t) env

analyzeTopDef :: (TopDef ErrPos) -> Analyzer ()
analyzeTopDef (FnDef _ t f args block) =
  -- TODO: check if argument names are unique.
  local (\env -> insertRet t (insertArgs args env)) $ analyzeBlock block

-- Change _declared in current block_ to false.
startBlock :: Env -> Env
startBlock env = M.map (\(_, t) -> (False, t)) env

analyzeBlock :: (Block ErrPos) -> Analyzer ()
analyzeBlock (Block _ ss) =
  local (\env -> startBlock env) $ analyzeManyStmt ss

analyzeManyStmt :: [Stmt ErrPos] -> Analyzer ()
analyzeManyStmt [] = return ()
analyzeManyStmt (s:ss) = do
  env <- analyzeStmt s
  local (\_ -> env) $ analyzeManyStmt ss

declare :: Type ErrPos -> Item ErrPos -> Analyzer Env
declare t (NoInit a x) = do
  env <- ask
  assertNotRedeclared a x
  return $ M.insert x (True, t) env
declare t (Init a x e) = do
  env <- ask
  assertNotRedeclared a x
  tt <- analyzeExpr e
  unless (t == tt) $ throwError $ typeMismatchError tt x t
  return $ M.insert x (True, t) env

analyzeStmt :: (Stmt ErrPos) -> Analyzer Env
analyzeStmt (Empty _) = ask
analyzeStmt (BStmt _ b) = analyzeBlock b >> ask
analyzeStmt (Decl _ t []) = ask
analyzeStmt (Decl _ t (x:xs)) = do
  env <- declare t x
  local (\_ -> env) $ analyzeStmt (Decl Nothing t xs)
analyzeStmt (Ass a x e) = do
  t <- mustLookup a x
  tt <- analyzeExpr e
  unless (t == tt) $ throwError $ typeMismatchError tt x t
  ask
-- TODO: REMOVE! Just for development
analyzeStmt _ = throwError $ "Not implemented yet!"
