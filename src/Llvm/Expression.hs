module Llvm.Expression where

import qualified Data.Map as M

import qualified Latte.AbsLatte as L

import Evaluate.Expression
import qualified Evaluate.Value as E

import Llvm.Compiler
import Llvm.Instruction

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
  Just (E.VInt n) -> return (Ti32, VInt $ -n)
  Nothing -> do
    (t, v) <- compileExpr e
    reg <- freshTemp
    emitInstruction $ IArithm Ti32 (VInt 0) v OpSub reg
    return (t, VReg reg)
compileExpr (L.Not _ e) = case tryEval e of
  Just (E.VBool b) -> return (Ti1, VBool $ not b)
  Nothing -> do
    (t, v) <- compileExpr e
    reg <- freshTemp
    emitInstruction $ IIcmp Ti1 (VBool False) v RelOpEQ reg
    return (t, VReg reg)
