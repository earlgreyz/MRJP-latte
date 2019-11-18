module Latte.ErrLatte where

-- Custom module for error position related stuff.
import Latte.AbsLatte

type ErrPos = Maybe (Int, Int)

showErrPos :: ErrPos -> String
showErrPos Nothing = ""
showErrPos (Just (l, p)) = (show l) ++ ":" ++ (show p)

getTypeErrPos :: Type ErrPos -> ErrPos
getTypeErrPos (Int e) = e
getTypeErrPos (Str e) = e
getTypeErrPos (Bool e) = e
getTypeErrPos (Void e) = e
getTypeErrPos (Fun e _ _) = e

getItemErrPos :: Item ErrPos -> ErrPos
getItemErrPos (NoInit e _) = e
getItemErrPos (Init e _ _) = e
