module Analyzer.Expression (analyzeExpr) where

import Control.Monad.Except
import Control.Monad.Reader

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.Assert
import Analyzer.Error
import Analyzer.Util

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
analyzeExpr (ELitInt a n) = do
  unless (n >= -2147483648 && n < 2147483647) $ throwError $ overflowError a n
  return $ Int a
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
analyzeExpr (EString a s) = do
  if (length s < 2) || (head s /= '\"') || (last s /= '\"') then
    throwError $ missingQuotesError a
  else
    return $ Str a
analyzeExpr (Neg a e) = analyzeUnaryExpr e (Int a)
analyzeExpr (Not a e) = analyzeUnaryExpr e (Bool a)
analyzeExpr (EMul a e _ f) = analyzeBinaryExpr e f (Int a)
analyzeExpr (EAdd a e op f) = case op of
  Plus _ -> do
    te <- analyzeExpr e
    assertOneOfType (getExprErrPos e) [Int Nothing, Str Nothing] te
    tf <- analyzeExpr f
    assertType (getExprErrPos f) te tf
    return $ fmap (\_ -> a) te
  Minus _ -> analyzeBinaryExpr e f (Int Nothing)
analyzeExpr (ERel a e op f) = case op of
  EQU _ -> do
    te <- analyzeExpr e
    tf <- analyzeExpr f
    assertType (getExprErrPos f) te tf
    return $ Bool a
  _ -> do
    analyzeBinaryExpr e f (Int Nothing)
    return $ Bool a
analyzeExpr (EAnd a e f) = analyzeBinaryExpr e f (Bool a)
analyzeExpr (EOr a e f) = analyzeBinaryExpr e f (Bool a)
