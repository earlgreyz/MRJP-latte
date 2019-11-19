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

functionError :: ErrPos -> Ident -> Type ErrPos -> String
functionError a f t = intercalate " " [
  (showErrPos a), "cannot use", (show f), (showErrPos $ getTypeErrPos t),
  "of type", (show t), "as a function"]

argumentsError :: ErrPos -> Ident -> [Type ErrPos] -> [Type ErrPos] -> String
argumentsError a f ts tts = intercalate " " [
  (showErrPos a), "invalid arguments for function", (show f),
  "expected", (show ts), "but got", (show tts), "instead"]

typeExpectedError :: ErrPos -> Type ErrPos -> Type ErrPos -> String
typeExpectedError a t tt = intercalate " " [
  (showErrPos a), "expected expression to be of type", (show t),
  "but got", (show tt), "instead"]

intExpectedError :: ErrPos -> Ident -> Type ErrPos -> String
intExpectedError a x t = intercalate " " [
  (showErrPos a), "expected variable to be of type int, but got",
  (show x), "of type", (show t)]
