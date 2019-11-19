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
