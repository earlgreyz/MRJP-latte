module Llvm.Internal where

import qualified Latte.AbsLatte as L

import Llvm.Llvm

stringsConcatIdent :: L.Ident
stringsConcatIdent = L.Ident "stringsConcat"
stringsConcat :: String
stringsConcat = "concat"

stringsEqualIdent :: L.Ident
stringsEqualIdent = L.Ident "stringsEqual"
stringsEqual :: String
stringsEqual = "streq"

internalFunctions :: [Declaration]
internalFunctions = [
  DeclFun Tvoid (L.Ident "printInt") "printInt" [Ti32],
  DeclFun Tvoid (L.Ident "printString") "printString" [Ptr Ti8],
  DeclFun Tvoid (L.Ident "error") "error" [],
  DeclFun Ti32 (L.Ident "readInt") "readInt" [],
  DeclFun (Ptr Ti8) (L.Ident "readString") "readString" [],
  DeclFun (Ptr Ti8) stringsConcatIdent stringsConcat [(Ptr Ti8), (Ptr Ti8)],
  DeclFun Ti1 stringsEqualIdent stringsEqual [(Ptr Ti8), (Ptr Ti8)]]
