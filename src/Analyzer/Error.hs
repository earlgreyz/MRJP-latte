module Analyzer.Error where

import Data.List

import Latte.AbsLatte
import Latte.ErrLatte
import Latte.PrintLatte

showIdent :: Ident -> String
showIdent (Ident str) = show str

redeclaredError :: ErrPos -> Ident -> ErrPos -> String
redeclaredError a x b = intercalate " " [
  (showErrPos a), (show x), "already declared in this block,",
  "previous declaration", (showErrPos b)]

typeMismatchError :: Print a => Type ErrPos -> a -> Type ErrPos -> String
typeMismatchError tt x t = intercalate " " [
  (showErrPos $ getTypeErrPos tt),
  "cannot assign expression of type", (show tt),
  "to", printTree x, "of type", (show t)]

undefinedError :: ErrPos -> Ident -> String
undefinedError a x = intercalate " " [
  (showErrPos a), (showIdent x), "was not defined in this scope"]

arrayError :: Print a => ErrPos -> a -> Type ErrPos -> String
arrayError a x t = intercalate " " [
  (showErrPos a), "expected array but got", printTree x, "of type", show t, "instead"]

functionError :: ErrPos -> Ident -> Type ErrPos -> String
functionError a f t = intercalate " " [
  (showErrPos a), "cannot use", (showIdent f), (showErrPos $ getTypeErrPos t),
  "of type", (show t), "as a function"]

argumentsError :: ErrPos -> Ident -> [Type ErrPos] -> [Type ErrPos] -> String
argumentsError a f ts tts = intercalate " " [
  (showErrPos a), "invalid arguments for function", (showIdent f),
  "expected", (show ts), "but got", (show tts), "instead"]

typeExpectedError :: ErrPos -> Type ErrPos -> Type ErrPos -> String
typeExpectedError a t tt = intercalate " " [
  (showErrPos a), "expected expression to be of type", (show t),
  "but got", (show tt), "instead"]

oneOfTypeExpectedError :: ErrPos -> [Type ErrPos] -> Type ErrPos -> String
oneOfTypeExpectedError a ts t = intercalate " " [
  (showErrPos a), "expected expression to be one of type", (show ts),
  "but got", (show t), "instead"]

intExpectedError :: Print a => ErrPos -> a -> Type ErrPos -> String
intExpectedError a x t = intercalate " " [
  (showErrPos a), "expected variable to be of type int, but got",
  (printTree x), "of type", (show t)]

missingReturnError :: ErrPos -> Ident -> String
missingReturnError a f = intercalate " " [
  (showErrPos a), "non-void function", (showIdent f), "missing a return statement"]

overflowError :: ErrPos -> Integer -> String
overflowError a n = intercalate " " [
  (showErrPos a), "invalid integer constant value", (show n), "(overflow)"]

missingQuotesError :: ErrPos -> String
missingQuotesError a = intercalate " " [
  (showErrPos a), "string constant missing quotes"]

voidArgumentError :: ErrPos -> Ident -> String
voidArgumentError a x = intercalate " " [
  (showErrPos a), "argument", (showIdent x), "has invalid type void"]

voidDeclarationError :: ErrPos -> String
voidDeclarationError a = (showErrPos a) ++ " void type cannot be used as a variable"

missingMainError :: String
missingMainError = "[0:0] missing main function"

invalidMainTypeError :: Type ErrPos -> String
invalidMainTypeError t = intercalate " " [
  (showErrPos $ getTypeErrPos t), "expected main to be of type int()",
  "but got", (show t), "instead"]
