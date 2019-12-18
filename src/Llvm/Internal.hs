module Llvm.Internal where

import qualified Latte.AbsLatte as L

import Llvm.Llvm

internalFunctions :: [Declaration]
internalFunctions = [
  DeclFun Tvoid (L.Ident "printInt") "printInt" [Ti32],
  DeclFun Tvoid (L.Ident "printString") "printString" [Ptr Ti8],
  DeclFun Tvoid (L.Ident "error") "error" [],
  DeclFun Ti32 (L.Ident "readInt") "readInt" [],
  DeclFun (Ptr Ti8) (L.Ident "readString") "readString" []]
