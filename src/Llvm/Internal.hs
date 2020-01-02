module Llvm.Internal where

import qualified Latte.AbsLatte as L

import Llvm.Llvm

stringsConcat :: String
stringsConcat = "stringsConcat"
stringsConcatIdent :: L.Ident
stringsConcatIdent = L.Ident stringsConcat

stringsEqual :: String
stringsEqual = "stringsEqual"
stringsEqualIdent :: L.Ident
stringsEqualIdent = L.Ident stringsEqual

malloc :: String
malloc = "malloc"
mallocIdent :: L.Ident
mallocIdent = L.Ident malloc

internalFunctions :: [Declaration]
internalFunctions = [
  DeclFun Tvoid (L.Ident "printInt") "printInt" [Ti32],
  DeclFun Tvoid (L.Ident "printString") "printString" [Ptr Ti8],
  DeclFun Tvoid (L.Ident "error") "error" [],
  DeclFun Ti32 (L.Ident "readInt") "readInt" [],
  DeclFun (Ptr Ti8) (L.Ident "readString") "readString" [],
  DeclFun (Ptr Ti8) stringsConcatIdent stringsConcat [(Ptr Ti8), (Ptr Ti8)],
  DeclFun Ti1 stringsEqualIdent stringsEqual [(Ptr Ti8), (Ptr Ti8)],
  DeclFun (Ptr Ti8) mallocIdent malloc [Ti32]]
