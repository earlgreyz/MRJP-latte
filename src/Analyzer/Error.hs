module Analyzer.Error where

import Data.List

import Latte.AbsLatte
import Latte.ErrLatte

redeclaredError :: ErrPos -> Ident -> ErrPos -> String
redeclaredError a x b = intercalate " " [
  (showErrPos a), (show x), "already declared in this block,",
  "previous declaration", (showErrPos b)]

typeMismatchError :: Type ErrPos -> Ident -> Type ErrPos -> String
typeMismatchError tt x t = intercalate " " [
  (showErrPos $ getTypeErrPos tt),
  "cannot assign expression of type", (show tt),
  "to", (show x), "of type", (show t)]

undefinedError :: ErrPos -> Ident -> String
undefinedError a x = intercalate " " [
  (showErrPos a), (show x), "was not defined in this scope"]
