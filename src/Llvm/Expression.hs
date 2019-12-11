module Llvm.Expression where

import qualified Data.Map as M

import qualified Latte.AbsLatte as L

import Evaluate.Expression
import qualified Evaluate.Value as E

import Llvm.Compiler
import Llvm.Instruction

joinIdent :: String
joinIdent = "join"

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
    emitInstruction $ IArithm (VInt 0) v OpSub reg
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
    emitInstruction $ IArithm v w (convertMulOp op) reg
    return (Ti32, VReg reg)
compileExpr expr@(L.EAdd _ e op f) = case tryEval expr of
  Just (E.VInt n) -> return (Ti32, VInt n)
  Just (E.VString s) -> do
    c <- newConstant s
    return (Ptr Ti8, VConst c)
  Nothing -> do
    (tv, v) <- compileExpr e
    (tw, w) <- compileExpr e
    reg <- freshTemp
    if tv == Ptr Ti8 then do
      -- `+` for strings concatanates them
      emitInstruction $ ICall (Ptr Ti8) joinIdent [(Ptr Ti8, v), (Ptr Ti8, w)] (Just reg)
      return (Ptr Ti8, VReg reg)
    else do
      emitInstruction $ IArithm v w (convertAddOp op) reg
      return (Ti32, VReg reg)

convertMulOp :: L.MulOp a -> ArithmOp
convertMulOp (L.Times _) = OpMul
convertMulOp (L.Div _) = OpDiv
convertMulOp (L.Mod _) = OpMod

convertAddOp :: L.AddOp a -> ArithmOp
convertAddOp (L.Plus _) = OpAdd
convertAddOp (L.Minus _) = OpSub
