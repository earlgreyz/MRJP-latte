module Llvm.Expression (compileExpr, getValuePointer, arrayOffset) where

import qualified Data.Map as M

import qualified Latte.AbsLatte as L

import Constexpr.Evaluate
import qualified Constexpr.Value as C

import Llvm.Compiler
import Llvm.Llvm
import Llvm.Internal
import Llvm.Util

-- Compiles expression simplifying constants.
compileExpr :: (L.Expr a) -> Compiler (Type, Value)
compileExpr e = case tryEval e of
  Nothing -> doCompileExpr e
  Just x -> case x of
    C.VInt n -> return (Ti32, VInt n)
    C.VBool b -> return (Ti1, VBool b)
    C.VString s -> newConstant s >>= \c -> return (Ptr Ti8, VConst c)

-- Calculates array offset.
arrayOffset :: Type -> Value -> Compiler Value
arrayOffset t index = case index of
  VInt n -> return (VInt $ (typeSize Ti32) + (typeSize t) * n)
  otherwise -> do
    tmp <- freshRegister
    emitInstruction $ IArithm OpMul index (VInt (typeSize $ t)) tmp
    -- First bytes are used to store the array size.
    reg <- freshRegister
    emitInstruction $ IArithm OpAdd (VReg tmp) (VInt (typeSize $ Ti32)) reg
    return $ VReg reg

-- Returns register storing lvalue pointer.
getValuePointer :: (L.LValue a) -> Compiler (Type, Register)
getValuePointer (L.LVar a x) = askVariables >>= \vs -> case M.lookup x vs of
  Just (t, r) -> return (t, r)
  Nothing -> getValuePointer $ L.LAttr a (L.EVar a $ L.LVar a selfIdent) x
getValuePointer (L.LAt _ x e) = do
  -- Index.
  (_, i) <- compileExpr e
  reg <- freshRegister
  -- Array.
  (at, array) <- compileExpr x
  let t = arrayType at
  -- Calculate element offset.
  offset <- arrayOffset t i
  element <- freshRegister
  emitInstruction $ IGetElementPtr Ti8 array offset element
  reg <- freshRegister
  emitInstruction $ IBitcast (Ptr Ti8) (VReg element) (Ptr t) reg
  return (t, reg)
getValuePointer (L.LAttr _ x a) = do
  (xt, ptr) <- compileExpr x
  case xt of
    Array t -> do
      reg <- freshRegister
      emitInstruction $ IBitcast (Ptr Ti8) ptr (Ptr Ti32) reg
      return (Ti32, reg)
    Object cls -> do
      cs <- askClasses
      -- Offset in the struct.
      let (_, _, as, _) = cs M.! cls
      let (t, offset) = as M.! a
      -- Pointer to the attribute.
      attribute <- freshRegister
      emitInstruction $ IGetElementPtr Ti8 ptr (VInt offset) attribute
      -- Cast attribute pointer to the actual type.
      reg <- freshRegister
      emitInstruction $ IBitcast (Ptr Ti8) (VReg attribute) (Ptr t) reg
      return (t, reg)

-- Actual expression compilation. Should not be called directly.
doCompileExpr :: (L.Expr a) -> Compiler (Type, Value)
doCompileExpr (L.EVar _ x) = do
  (t, ptr) <- getValuePointer x
  reg <- freshRegister
  emitInstruction $ ILoad t (VReg ptr) reg
  return (t, VReg reg)
doCompileExpr (L.EApp _ f args) = do
  fs <- askFunctions
  xs <- mapM compileExpr args
  let (rt, fname) = fs M.! f
  case rt of
    Tvoid -> do
      emitInstruction $ ICall rt fname xs Nothing
      return (Tvoid, VInt 0)
    _ -> do
      reg <- freshRegister
      emitInstruction $ ICall rt fname xs (Just reg)
      return (rt, VReg reg)
doCompileExpr (L.EAttrFun _ obj f args) = do
  (t, v) <- compileExpr obj
  xs <- mapM compileExpr args
  cs <- askClasses
  let (_, _, _, ms) = cs M.! (className t)
  let (rt, fname) = ms M.! f
  case rt of
    Tvoid -> do
      emitInstruction $ ICall rt fname ((t, v):xs) Nothing
      return (Tvoid, VInt 0)
    _ -> do
      reg <- freshRegister
      emitInstruction $ ICall rt fname ((t, v):xs) (Just reg)
      return (rt, VReg reg)
doCompileExpr (L.Neg _ e) = do
  (_, v) <- compileExpr e
  reg <- freshRegister
  emitInstruction $ IArithm OpSub (VInt 0) v reg
  return (Ti32, VReg reg)
doCompileExpr (L.Not _ e) = do
  (_, v) <- compileExpr e
  reg <- freshRegister
  emitInstruction $ IIcmp CondEQ Ti1 (VBool False) v reg
  return (Ti1, VReg reg)
doCompileExpr (L.EMul _ e op f) = do
  (_, v) <- compileExpr e
  (_, w) <- compileExpr f
  reg <- freshRegister
  emitInstruction $ IArithm (convertMulOp op) v w reg
  return (Ti32, VReg reg)
  where
    convertMulOp :: L.MulOp a -> ArithmOp
    convertMulOp op = case op of
      L.Times _ -> OpMul
      L.Div _ -> OpDiv
      L.Mod _ -> OpMod
doCompileExpr (L.EAdd _ e op f) = do
  (tv, v) <- compileExpr e
  (tw, w) <- compileExpr f
  reg <- freshRegister
  -- Strings `+` has to be handled seperately.
  if tv == Ptr Ti8 then do
    emitInstruction $ ICall (Ptr Ti8) stringsConcat [(Ptr Ti8, v), (Ptr Ti8, w)] (Just reg)
    return (Ptr Ti8, VReg reg)
  else do
    emitInstruction $ IArithm (convertAddOp op) v w reg
    return (Ti32, VReg reg)
  where
    convertAddOp :: L.AddOp a -> ArithmOp
    convertAddOp op = case op of
      L.Plus _ -> OpAdd
      L.Minus _ -> OpSub
doCompileExpr (L.ERel _ e op f) = do
  (tv, v) <- compileExpr e
  (tw, w) <- compileExpr f
  reg <- freshRegister
  -- Strings comparison has to be handled seperately.
  if tv == Ptr Ti8 then do
    emitInstruction $ ICall (Ptr Ti8) stringsEqual [(Ptr Ti8, v), (Ptr Ti8, w)] (Just reg)
    -- If `\=` we need to negate the result.
    case op of
      L.NE _ -> do
        res <- freshRegister
        emitInstruction $ IIcmp CondEQ Ti1 (VBool False) (VReg reg) res
        return (Ti1, VReg res)
      _ -> return (Ti1, VReg reg)
  else do
    emitInstruction $ IIcmp (convertRelOp op) tv v w reg
    return (Ti1, VReg reg)
  where
    convertRelOp :: L.RelOp a -> Cond
    convertRelOp op = case op of
      L.EQU _ -> CondEQ
      L.NE _ -> CondNE
      L.LTH _ -> CondSLT
      L.LE _ -> CondSLE
      L.GTH _ -> CondSGT
      L.GE _ -> CondSGE
doCompileExpr (L.EAnd _ e f) = do
  vlabel <- freshLabel
  wlabel <- freshLabel
  retlabel <- freshLabel
  emitInstruction $ IBr vlabel
  -- First operand in `and`.
  emitInstruction $ ILabel vlabel
  (_, v) <- compileExpr e
  evlabel <- getBlockLabel
  emitInstruction $ IBrCond v wlabel retlabel
  -- Second operand in `and`.
  emitInstruction $ ILabel wlabel
  (_, w) <- compileExpr f
  ewlabel <- getBlockLabel
  emitInstruction $ IBr retlabel
  -- Calculate `and` result.
  emitInstruction $ ILabel retlabel
  reg <- freshRegister
  emitInstruction $ IPhi Ti1 [(VBool False, evlabel), (w, ewlabel)] reg
  return (Ti1, VReg reg)
doCompileExpr (L.EOr _ e f) = do
  vlabel <- freshLabel
  wlabel <- freshLabel
  retlabel <- freshLabel
  emitInstruction $ IBr vlabel
  -- First operand in `and`.
  emitInstruction $ ILabel vlabel
  (_, v) <- compileExpr e
  evlabel <- getBlockLabel
  emitInstruction $ IBrCond v retlabel wlabel
  -- Second operand in `and`.
  emitInstruction $ ILabel wlabel
  (_, w) <- compileExpr f
  ewlabel <- getBlockLabel
  emitInstruction $ IBr retlabel
  -- Calculate `and` result.
  emitInstruction $ ILabel retlabel
  reg <- freshRegister
  emitInstruction $ IPhi Ti1 [(VBool True, evlabel), (w, ewlabel)] reg
  return (Ti1, VReg reg)
doCompileExpr (L.ENewArr _ t e) = do
  (_, len) <- compileExpr e
  let innerType = convertType t
  -- Allocate array as bytes to store the size in front of the data.
  size <- arrayOffset innerType len
  array <- freshRegister
  emitInstruction $ ICall (Ptr Ti8) malloc [(Ti32, size)] (Just array)
  -- Store the size.
  sizeLocation <- freshRegister
  emitInstruction $ IBitcast (Ptr Ti8) (VReg array) (Ptr Ti32) sizeLocation
  emitInstruction $ IStore Ti32 len sizeLocation
  return (Array innerType, VReg array)
doCompileExpr (L.ENewObj _ cls) = do
  cs <- askClasses
  let (size, name, _, _) = cs M.! cls
  object <- freshRegister
  emitInstruction $ ICall (Ptr Ti8) malloc [(Ti32, VInt size)] (Just object)
  -- Store the class name at the beggining of the struct.
  objectName <- freshRegister
  emitInstruction $ IBitcast (Ptr Ti8) (VReg object) (Ptr $ Ptr Ti8) objectName
  emitInstruction $ IStore (Ptr Ti8) (VConst name) objectName
  return (Object cls, VReg object)
doCompileExpr (L.ENullCast _ cls) = do
  return (Object cls, VNull)
doCompileExpr (L.ECast _ _ x) = do
  (t, r) <- compileExpr x
  return (t, r)
