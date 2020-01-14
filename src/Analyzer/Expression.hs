module Analyzer.Expression (analyzeExpr, analyzeLValue) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.Assert
import Analyzer.Error
import Analyzer.Internal
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

analyzeLValue :: (LValue ErrPos) -> Analyzer (Type ErrPos)
analyzeLValue (LVar a x) = mustLookup a x
analyzeLValue (LAt a x i) = do
  it <- analyzeExpr i
  assertType a (Int Nothing) it
  xt <- analyzeExpr x
  case xt of
    Array _ t -> return t
    otherwise -> throwError $ arrayError a x xt
analyzeLValue (LAttr a x f) = analyzeExpr x >>= \xt -> case xt of
  Class _ cls -> mustLookupField a cls f
  Array _ t -> do
    if f == lengthIdent then
      return $ Int a
    else
      throwError $ attributeObjectError a xt f
  otherwise -> throwError $ attributeObjectError a xt f

analyzeExpr :: (Expr ErrPos) -> Analyzer (Type ErrPos)
analyzeExpr (EVar _ x) = analyzeLValue x
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
analyzeExpr (EAttrFun a obj f args) = do
  t <- analyzeExpr obj
  case t of
    Class _ cls -> do
      fun <- mustLookupField a cls f
      case fun of
        Fun _ r ts -> do
          tts <- mapM analyzeExpr args
          unless (ts == tts) $ throwError $ argumentsError a f ts tts
          return r
        tt -> throwError $ functionError a f tt
    otherwise -> throwError $ methodError a t
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
  NE _ -> analyzeExpr (ERel a e (EQU a) f)
  _ -> do
    analyzeBinaryExpr e f (Int Nothing)
    return $ Bool a
analyzeExpr (EAnd a e f) = analyzeBinaryExpr e f (Bool a)
analyzeExpr (EOr a e f) = analyzeBinaryExpr e f (Bool a)
analyzeExpr (ENewArr a t e) = do
  te <- analyzeExpr e
  assertType a (Int Nothing) te
  return $ Array a t
analyzeExpr (ENewObj a cls) = do
  mustLookupClass a cls
  return $ Class a cls
analyzeExpr (ENullCast a cls) = do
  mustLookupClass a cls
  return $ Class a cls
