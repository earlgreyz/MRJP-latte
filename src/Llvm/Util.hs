module Llvm.Util where

import qualified Latte.AbsLatte as L

import Llvm.Llvm

convertType :: L.Type a -> Type
convertType t = case t of
  L.Void _ -> Tvoid
  L.Int _ -> Ti32
  L.Bool _ -> Ti1
  L.Str _ -> Ptr Ti8
  L.Array _ t -> Array $ convertType t
  L.Class _ cls -> Object cls

typeSize :: Type -> Integer
typeSize t = case t of
  Ti64 -> 8
  Ti32 -> 4
  Ti8 -> 1
  Ti1 -> 1
  Ptr _ -> typeSize Ti64
  Array _ -> typeSize Ti64
  Object _ -> typeSize Ti64
  Tvoid -> error "void type has no size"

arrayType :: Type -> Type
arrayType t = case t of
  Array t -> t
  otherwise -> error "expected array type"

className :: Type -> L.Ident
className t = case t of
  Object cls -> cls
  otherwise -> error "expected object type"

convertFunctionName :: L.Ident -> String
convertFunctionName (L.Ident s) =
  if s == "main" then s else "_" ++ s

isFinalInstruction :: Instruction -> Bool
isFinalInstruction i = case i of
  IBr _ -> True
  IBrCond _ _ _ -> True
  IRet _ _ -> True
  otherwise -> False

fmapInstructionValue :: Instruction -> (Value -> Value) -> Instruction
fmapInstructionValue i f = case i of
  ICall t s args r -> ICall t s (flip map args $ \(t, v) -> (t, f v)) r
  IRet t v -> IRet t (f v)
  IArithm o v w r -> IArithm o (f v) (f w) r
  IBrCond v l m -> IBrCond (f v) l m
  ILoad t v r -> ILoad t (f v) r
  IStore t v r -> IStore t (f v) r
  IIcmp c t v w r -> IIcmp c t (f v) (f w) r
  IPhi t args r ->  IPhi t (flip map args $ \(v, l) -> (f v, l)) r
  IBitcast t v tt r -> IBitcast t (f v) tt r
  IGetElementPtr t v w r -> IGetElementPtr t (f v) (f w) r
  i -> i
