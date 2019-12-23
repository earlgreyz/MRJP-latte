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

isFinalInstruction :: Instruction -> Bool
isFinalInstruction i = case i of
  IBr _ -> True
  IBrCond _ _ _ -> True
  IRet _ _ -> True
  otherwise -> False

-- Composition of a list of functions.
compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- Finds a function fix point.
fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f x = let y = f x in
  if x == y then x else fixPoint f y
