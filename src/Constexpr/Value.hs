module Constexpr.Value where

data Value
  = VInt Integer
  | VBool Bool
  | VString String

instance Eq Value where
  (VInt n) == (VInt m) = n == m
  (VBool b) == (VBool c) = b == c
  (VString s) == (VString t) = s == t
  _ == _ = False
