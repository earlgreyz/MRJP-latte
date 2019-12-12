module Llvm.Expression where

import qualified Data.Map as M

import qualified Latte.AbsLatte as L

import Evaluate.Expression
import qualified Evaluate.Value as E

import Llvm.Compiler
import Llvm.Instruction

stringsConcatIdent :: String
stringsConcatIdent = "stringsConcat"

stringsEqualIdent :: String
stringsEqualIdent = "stringsEqual"

compileExpr :: (L.Expr a) -> Compiler (Type, Value)
compileExpr (L.EVar _ x) = do
  vs <- askVariables
  reg <- freshTemp
  let (t, ptr) = vs M.! x
  emitInstruction $ ILoad t ptr reg
  return (t, VReg reg)
compileExpr (L.ELitInt _ n) = return (Ti32, VInt n)
compileExpr (L.ELitTrue _) = return (Ti1, VBool True)
compileExpr (L.ELitFalse _) = return (Ti1, VBool False)
compileExpr (L.EApp _ f args) = do
  fs <- askFunctions
  xs <- mapM compileExpr args
  let (rt, _) = fs M.! f
  case rt of
    Tvoid -> do
      emitInstruction $ ICall rt (show f) xs Nothing
      return (Tvoid, VInt 0)
    _ -> do
      reg <- freshTemp
      emitInstruction $ ICall rt (show f) xs (Just reg)
      return (rt, VReg reg)
compileExpr (L.EString _ s) = do
  c <- newConstant s
  return (Ptr Ti8, VConst c)
compileExpr (L.Neg _ e) = case tryEval e of
  Just (E.VInt n) -> return (Ti32, VInt n)
  Nothing -> do
    (_, v) <- compileExpr e
    reg <- freshTemp
    emitInstruction $ IArithm OpSub (VInt 0) v reg
    return (Ti32, VReg reg)
compileExpr (L.Not _ e) = case tryEval e of
  Just (E.VBool b) -> return (Ti1, VBool b)
  Nothing -> do
    (_, v) <- compileExpr e
    reg <- freshTemp
    emitInstruction $ IIcmp CondEQ Ti1 (VBool False) v reg
    return (Ti1, VReg reg)
compileExpr expr@(L.EMul _ e op f) = case tryEval expr of
  Just (E.VInt n) -> return (Ti32, VInt n)
  Nothing -> do
    (_, v) <- compileExpr e
    (_, w) <- compileExpr e
    reg <- freshTemp
    emitInstruction $ IArithm (convertMulOp op) v w reg
    return (Ti32, VReg reg)
  where
    convertMulOp :: L.MulOp a -> ArithmOp
    convertMulOp op = case op of
      L.Times _ -> OpMul
      L.Div _ -> OpDiv
      L.Mod _ -> OpMod
compileExpr expr@(L.EAdd _ e op f) = case tryEval expr of
  Just (E.VInt n) -> return (Ti32, VInt n)
  Just (E.VString s) -> do
    c <- newConstant s
    return (Ptr Ti8, VConst c)
  Nothing -> do
    (tv, v) <- compileExpr e
    (tw, w) <- compileExpr e
    reg <- freshTemp
    -- Strings `+` has to be handled seperately.
    if tv == Ptr Ti8 then
      emitInstruction $ ICall (Ptr Ti8) stringsConcatIdent [(Ptr Ti8, v), (Ptr Ti8, w)] (Just reg)
    else
      emitInstruction $ IArithm (convertAddOp op) v w reg
    return (Ti32, VReg reg)
    where
      convertAddOp :: L.AddOp a -> ArithmOp
      convertAddOp op = case op of
        L.Plus _ -> OpAdd
        L.Minus _ -> OpSub
compileExpr expr@(L.ERel _ e op f) = case tryEval expr of
  Just (E.VBool b) -> return (Ti1, VBool b)
  Nothing -> do
    (tv, v) <- compileExpr e
    (tw, w) <- compileExpr e
    reg <- freshTemp
    -- Strings comparison has to be handled seperately.
    if tv == Ptr Ti8 then do
      emitInstruction $ ICall (Ptr Ti8) stringsEqualIdent [(Ptr Ti8, v), (Ptr Ti8, w)] (Just reg)
      -- If `\=` we need to negate the result.
      case op of
        L.NE _ -> do
          res <- freshTemp
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
