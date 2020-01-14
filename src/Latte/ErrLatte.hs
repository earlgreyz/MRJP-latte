module Latte.ErrLatte where

-- Custom module for error position related stuff.
import Latte.AbsLatte

type ErrPos = Maybe (Int, Int)

showErrPos :: ErrPos -> String
showErrPos Nothing = ""
showErrPos (Just (l, p)) = "[" ++ (show l) ++ ":" ++ (show p) ++ "]"

getTypeErrPos :: Type a -> a
getTypeErrPos (Int a) = a
getTypeErrPos (Str a) = a
getTypeErrPos (Bool a) = a
getTypeErrPos (Void a) = a
getTypeErrPos (Array a _) = a
getTypeErrPos (Class a _) = a
getTypeErrPos (Fun a _ _) = a

getItemErrPos :: Item a -> a
getItemErrPos (NoInit a _) = a
getItemErrPos (Init a _ _) = a

getExprErrPos :: Expr a -> a
getExprErrPos (EVar a _) = a
getExprErrPos (ELitInt a _) = a
getExprErrPos (ELitTrue a) = a
getExprErrPos (ELitFalse a) = a
getExprErrPos (EApp a _ _) = a
getExprErrPos (ENewArr a _ _) = a
getExprErrPos (ENewObj a _) = a
getExprErrPos (EAttrFun a _ _ _) = a
getExprErrPos (ENullCast a _) = a
getExprErrPos (ECast a _ _) = a
getExprErrPos (EString a _) = a
getExprErrPos (Neg a _) = a
getExprErrPos (Not a _) = a
getExprErrPos (EMul a _ _ _) = a
getExprErrPos (EAdd a _ _ _) = a
getExprErrPos (ERel a _ _ _) = a
getExprErrPos (EAnd a _ _) = a
getExprErrPos (EOr a _ _) = a
