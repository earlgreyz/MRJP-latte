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

lengthIdent :: L.Ident
lengthIdent = L.Ident "length"

selfIdent :: L.Ident
selfIdent = L.Ident "self"

internalFunctions :: [Declaration]
internalFunctions = [
  Declaration Tvoid (L.Ident "printInt") "printInt" [Ti32],
  Declaration Tvoid (L.Ident "printString") "printString" [Ptr Ti8],
  Declaration Tvoid (L.Ident "error") "error" [],
  Declaration Ti32 (L.Ident "readInt") "readInt" [],
  Declaration (Ptr Ti8) (L.Ident "readString") "readString" [],
  Declaration (Ptr Ti8) stringsConcatIdent stringsConcat [(Ptr Ti8), (Ptr Ti8)],
  Declaration Ti1 stringsEqualIdent stringsEqual [(Ptr Ti8), (Ptr Ti8)],
  Declaration (Ptr Ti8) mallocIdent malloc [Ti32]]
