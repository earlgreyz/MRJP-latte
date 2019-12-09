module Evaluate.Value where

data Value
  = VInt Integer
  | VBool Bool
  | VString String

instance Eq Value where
  (VInt n) == (VInt m) = n == m
  (VBool b) == (VBool c) = b == c
  (VString s) == (VString t) = s == t
  _ == _ = False

(|+|) :: Value -> Value -> Maybe Value
(VInt n) |+| (VInt m) = Just $ VInt $ n + m
(VString s) |+| (VString t) = Just $ VString $ s ++ t
_ |+| _ = Nothing

requireInt :: Value -> Maybe Integer
requireInt (VInt n) = return n
requireInt _ = Nothing

requireBool :: Value -> Maybe Bool
requireBool (VBool b) = return b
requireBool _ = Nothing

requireString :: Value -> Maybe String
requireString (VString s) = return s
requireString _ = Nothing
