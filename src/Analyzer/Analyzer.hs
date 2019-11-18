module Analyzer.Analyzer where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader
import Data.List

import Latte.AbsLatte
import Latte.ErrLatte

type AnalyzerType = (Bool, Type ErrPos)
type Env = M.Map Ident AnalyzerType
type Analyzer = ReaderT Env (ExceptT String IO)

-- Special markers for the environment.
returnIdent :: Ident
returnIdent = Ident "return"

-- Helper function to show errors.
throwAnalyzerError :: ErrPos -> String -> Analyzer a
throwAnalyzerError a s = throwError $ (showErrPos a) ++ s

throwRedeclaredError :: ErrPos -> Ident -> ErrPos -> Analyzer a
throwRedeclaredError a x b = throwError $ intercalate " " [
  (showErrPos a), (show x),
  "already declared in this block, previous declaration", (showErrPos b)]

throwTypeMismatchError :: Type ErrPos -> Ident -> Type ErrPos -> Analyzer a
throwTypeMismatchError tt x t = throwError $ intercalate " " [
  (showErrPos $ getTypeErrPos tt),
  "cannot assign expression of type", (show tt),
  "to", (show x), "of type", (show t)]

-- Runs analyzer with starting environment.
runAnalyzer :: (Program ErrPos) -> ExceptT String IO ()
runAnalyzer p = runReaderT (analyze p) M.empty

-- Analyzer functions.
analyze :: (Program ErrPos) -> Analyzer ()
analyze (Program _ ts) = mapM_ analyzeTopDef ts

-- Insert arguments into the environment.
insertArgs :: [Arg ErrPos] -> Env -> Env
insertArgs args env = foldr insertArg env args where
  insertArg (Arg _ t arg) env = M.insert arg (True, t) env

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

checkNotRedeclared :: ErrPos -> Ident -> Analyzer ()
checkNotRedeclared a x = do
  env <- ask
  case M.lookup x env of
    Just (True, tt) -> throwRedeclaredError a x (getTypeErrPos tt)
    _ -> return ()

declare :: Type ErrPos -> Item ErrPos -> Analyzer Env
declare t (NoInit a x) = do
  env <- ask
  checkNotRedeclared a x
  return $ M.insert x (True, t) env
declare t (Init a x e) = do
  env <- ask
  checkNotRedeclared a x
  tt <- analyzeExpr e
  unless (t == tt) $ throwTypeMismatchError tt x t
  return $ M.insert x (True, t) env

analyzeStmt :: (Stmt ErrPos) -> Analyzer Env
analyzeStmt (Empty _) = ask
analyzeStmt (BStmt _ b) = analyzeBlock b >> ask
analyzeStmt (Decl _ t []) = ask
analyzeStmt (Decl a t (x:xs)) = do
  env <- declare t x
  local (\_ -> env) $ analyzeStmt (Decl a t xs)
-- TODO: REMOVE! Just for development
analyzeStmt _ = throwError $ "Not implemented yet!"

analyzeExpr :: (Expr ErrPos) -> Analyzer (Type ErrPos)
-- TODO: REMOVE! Just for development
analyzeExpr _ = throwError $ "Not implemented yet!"
