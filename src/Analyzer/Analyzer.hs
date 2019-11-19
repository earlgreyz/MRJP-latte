module Analyzer.Analyzer where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader
import Data.List

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Error

type AnalyzerType = (Bool, Type ErrPos)
type Env = M.Map Ident AnalyzerType
type Analyzer = ReaderT Env (ExceptT String IO)

-- Special markers for the environment.
returnIdent :: Ident
returnIdent = Ident "return"

-- Helper function to show errors.
throwAnalyzerError :: ErrPos -> String -> Analyzer a
throwAnalyzerError a s = throwError $ (showErrPos a) ++ s

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

-- Assert identifier has not been declared in the current block.
assertNotRedeclared :: ErrPos -> Ident -> Analyzer ()
assertNotRedeclared a x = do
  env <- ask
  case M.lookup x env of
    Just (True, tt) -> throwError $ redeclaredError a x (getTypeErrPos tt)
    _ -> return ()

-- Add variable to the current environment.
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

mustLookup :: ErrPos -> Ident -> Analyzer (Type ErrPos)
mustLookup a x = do
  env <- ask
  case M.lookup x env of
    Nothing -> throwError $ undefinedError a x
    Just (_, t) -> return t

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

-- Assert type matches the required type.
assertType :: ErrPos -> Type ErrPos -> Type ErrPos -> Analyzer ()
assertType a t tt = do
  unless (t == tt) $ throwError $ typeExpectedError a t tt

analyzeUnaryExpr :: Expr ErrPos -> Type ErrPos -> Analyzer (Type ErrPos)
analyzeUnaryExpr e t = do
  tt <- analyzeExpr e
  assertType (getExprErrPos e) t tt
  return tt

analyzeBinaryExpr :: Expr ErrPos -> Expr ErrPos -> Type ErrPos -> Analyzer (Type ErrPos)
analyzeBinaryExpr e f t = do
  te <- analyzeExpr e
  assertType (getExprErrPos e) t te
  tf <- analyzeExpr f
  assertType (getExprErrPos f) t tf
  return te

analyzeExpr :: (Expr ErrPos) -> Analyzer (Type ErrPos)
analyzeExpr (EVar a x) = mustLookup a x
analyzeExpr (ELitInt a _) = return $ Int a
analyzeExpr (ELitTrue a) = return $ Bool a
analyzeExpr (ELitFalse a) = return $ Bool a
analyzeExpr (EApp a f args) = do
  fun <- mustLookup a f
  case fun of
    Fun _ r ts -> do
      tts <- mapM analyzeExpr args
      unless (ts == tts) $ throwError $ argumentsError a f ts tts
      return r
    tt -> throwError $ functionError a f tt
analyzeExpr (EString a _) = return $ Str a
analyzeExpr (Neg _ e) = analyzeUnaryExpr e (Int Nothing)
analyzeExpr (Not _ e) = analyzeUnaryExpr e (Bool Nothing)
analyzeExpr (EMul _ e _ f) = analyzeBinaryExpr e f (Int Nothing)
analyzeExpr (EAdd _ e _ f) = analyzeBinaryExpr e f (Int Nothing)
analyzeExpr (ERel _ e _ f) = do
  t <- analyzeBinaryExpr e f (Int Nothing)
  return $ Bool (getTypeErrPos t)
analyzeExpr (EAnd _ e f) = analyzeBinaryExpr e f (Bool Nothing)
analyzeExpr (EOr _ e f) = analyzeBinaryExpr e f (Bool Nothing)
