module Constexpr.Util where

import Constexpr.Value

requireSameType :: Value -> Value -> Maybe ()
requireSameType (VInt _) (VInt _) = return ()
requireSameType (VBool _) (VBool _) = return ()
requireSameType (VString _) (VString _) = return ()
requireSameType _ _ = Nothing

requireInt :: Value -> Maybe Integer
requireInt (VInt n) = return n
requireInt _ = Nothing

requireBool :: Value -> Maybe Bool
requireBool (VBool b) = return b
requireBool _ = Nothing

requireString :: Value -> Maybe String
requireString (VString s) = return s
requireString _ = Nothing
