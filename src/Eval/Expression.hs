module Eval.Expression where

import Control.Monad
import Data.Maybe

import Latte.AbsLatte

data EValue = EVInt Integer | EVBool Bool | EVString String

instance Eq EValue where
  (EVInt n) == (EVInt m) = n == m
  (EVBool b) == (EVBool c) = b == c
  (EVString s) == (EVString t) = s == t
  _ == _ = False

requireInt :: EValue -> Maybe Integer
requireInt (EVInt n) = return n
requireInt _ = Nothing

requireBool :: EValue -> Maybe Bool
requireBool (EVBool b) = return b
requireBool _ = Nothing

requireString :: EValue -> Maybe String
requireString (EVString s) = return s
requireString _ = Nothing

assertTypeMatches :: EValue -> EValue -> Maybe ()
assertTypeMatches (EVInt _) (EVInt _) = return ()
assertTypeMatches (EVBool _) (EVBool _) = return ()
assertTypeMatches (EVString _) (EVString _) = return ()
assertTypeMatches _ _ = Nothing

addValues :: EValue -> EValue -> Maybe EValue
addValues (EVInt n) (EVInt m) = return $ EVInt $ n + m
addValues (EVString s) (EVString t) = return $ EVString $ s ++ t
addValues _ _ = Nothing

cmpInts :: (Integer -> Integer -> Bool) -> EValue -> EValue -> Maybe EValue
cmpInts c (EVInt n) (EVInt m) = return $ EVBool $ c n m
cmpInts _ _ _= Nothing

tryEval :: (Expr a) -> Maybe EValue
tryEval (EVar _ _) = Nothing
tryEval (ELitInt _ n) = return $ EVInt n
tryEval (ELitTrue _) = return $ EVBool True
tryEval (ELitFalse _) = return $ EVBool False
tryEval (EApp _ _ _) = Nothing
tryEval (EString _ s) = return $ EVString s
tryEval (Neg _ e) = tryEval e >>= \v -> requireInt v >>= \n -> return $ EVInt $ -n
tryEval (Not _ e) = tryEval e >>= \v -> requireBool v >>= \b -> return $ EVBool $ not b
tryEval (EMul _ e op f) = do
  v <- tryEval e
  n <- requireInt v
  w <- tryEval f
  m <- requireInt w
  case op of
    Times _ -> return $ EVInt $ n * m
    Div _ -> return $ EVInt $ n `div` m
    Mod _ -> return $ EVInt $ n `mod` m
tryEval (EAdd _ e op f) = case op of
  Plus _ -> do
    v <- tryEval e
    w <- tryEval f
    addValues v w
  Minus _ -> do
    v <- tryEval e
    n <- requireInt v
    w <- tryEval f
    m <- requireInt w
    return $ EVInt $ n - m
tryEval (ERel _ e op f) = do
  v <- tryEval e
  w <- tryEval f
  case op of
    EQU _ -> do
      assertTypeMatches v w
      return $ EVBool $ v == w
    NE _ -> do
      assertTypeMatches v w
      return $ EVBool $ v /= w
    LTH _ -> cmpInts (<) v w
    LE _ -> cmpInts (<=) v w
    GTH _ -> cmpInts (>) v w
    GE _ -> cmpInts (>=) v w
tryEval (EAnd _ e f) = do
  v <- tryEval e
  b <- requireBool v
  w <- tryEval f
  c <- requireBool w
  return $ EVBool $ b && c
tryEval (EOr _ e f) = do
  v <- tryEval e
  b <- requireBool v
  w <- tryEval f
  c <- requireBool w
  return $ EVBool $ b || c
