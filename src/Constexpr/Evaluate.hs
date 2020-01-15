module Constexpr.Evaluate (tryEval) where

import Control.Monad
import Data.Maybe

import Util.String

import Latte.AbsLatte

import Constexpr.Util
import Constexpr.Value

cmpInts :: (Integer -> Integer -> Bool) -> Value -> Value -> Maybe Value
cmpInts c (VInt n) (VInt m) = return $ VBool $ c n m
cmpInts _ _ _= Nothing

tryEval :: (Expr a) -> Maybe Value
tryEval (EVar _ _) = Nothing
tryEval (ELitInt _ n) = return $ VInt n
tryEval (ELitTrue _) = return $ VBool True
tryEval (ELitFalse _) = return $ VBool False
tryEval (EApp _ _ _) = Nothing
tryEval (EAttrFun _ _ _ _) = Nothing
tryEval (EString a s) = return $ VString (unquote s)
tryEval (Neg _ e) = tryEval e >>= \v -> requireInt v >>= \n -> return $ VInt $ -n
tryEval (Not _ e) = tryEval e >>= \v -> requireBool v >>= \b -> return $ VBool $ not b
tryEval (EMul _ e op f) = do
  v <- tryEval e
  n <- requireInt v
  w <- tryEval f
  m <- requireInt w
  case op of
    Times _ -> return $ VInt $ n * m
    Div _ -> return $ VInt $ n `div` m
    Mod _ -> return $ VInt $ n `mod` m
tryEval (EAdd _ e op f) = case op of
  Plus _ -> do
    v <- tryEval e
    w <- tryEval f
    case (v, w) of
      (VInt n, VInt m) -> return $ VInt $ n + m
      (VString s, VString t) -> return $ VString $ s ++ t
      _ -> Nothing
  Minus _ -> do
    v <- tryEval e
    n <- requireInt v
    w <- tryEval f
    m <- requireInt w
    return $ VInt $ n - m
tryEval (ERel _ e op f) = do
  v <- tryEval e
  w <- tryEval f
  case op of
    EQU _ -> do
      requireSameType v w
      return $ VBool $ v == w
    NE _ -> do
      requireSameType v w
      return $ VBool $ v /= w
    LTH _ -> cmpInts (<) v w
    LE _ -> cmpInts (<=) v w
    GTH _ -> cmpInts (>) v w
    GE _ -> cmpInts (>=) v w
tryEval (EAnd _ e f) = do
  v <- tryEval e
  b <- requireBool v
  w <- tryEval f
  c <- requireBool w
  return $ VBool $ b && c
tryEval (EOr _ e f) = do
  v <- tryEval e
  b <- requireBool v
  w <- tryEval f
  c <- requireBool w
  return $ VBool $ b || c
tryEval (ENewArr _ _ _) = Nothing
tryEval (ENewObj _ _) = Nothing
tryEval (ENullCast _ _) = Nothing
tryEval (ECast _ _ _) = Nothing
