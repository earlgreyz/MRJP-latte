module Llvm.Util where

import qualified Latte.AbsLatte as L

import Llvm.Llvm

convertType :: L.Type a -> Type
convertType t = case t of
  L.Void _ -> Tvoid
  L.Int _ -> Ti32
  L.Bool _ -> Ti1
  L.Str _ -> Ptr Ti8

convertFunctionName :: L.Ident -> String
convertFunctionName (L.Ident s) =
  if s == "main" then s else "_" ++ s

-- Helper functions.
isFinalInstruction :: Instruction -> Bool
isFinalInstruction i = case i of
  IBr _ -> True
  IBrCond _ _ _ -> True
  IRet _ _ -> True
  otherwise -> False
