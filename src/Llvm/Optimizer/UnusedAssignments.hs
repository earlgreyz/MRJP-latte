module Llvm.Optimizer.UnusedAssignments (runRemoveUnusedAssignments) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import Llvm.Llvm

-- Registers accessed in the instruction.
accessedRegisters :: Instruction -> S.Set Register
accessedRegisters i = case i of
  ICall _ _ args _ -> S.fromList $ mapMaybe (toRegister . snd) args
  IRet _ x -> S.fromList $ mapMaybe toRegister [x]
  IArithm _ x y _ -> S.fromList $ mapMaybe toRegister [x, y]
  IBrCond x _ _ -> S.fromList $ mapMaybe toRegister [x]
  ILoad _ x _ -> S.fromList $ mapMaybe toRegister [x]
  IStore _ x r -> S.fromList (r:mapMaybe toRegister [x])
  IIcmp _ _ x y _ -> S.fromList $ mapMaybe toRegister [x, y]
  IPhi _ args _ -> S.fromList $ mapMaybe (toRegister . fst) args
  IBitcast _ x _ _ -> S.fromList $ mapMaybe toRegister [x]
  IGetElementPtr _ x y _ -> S.fromList $ mapMaybe toRegister [x, y]
  otherwise -> S.empty
  where
    toRegister :: Value -> Maybe Register
    toRegister (VReg r) = return r
    toRegister _ = Nothing

-- Registers accessed in the block.
blockAccessedRegisters :: Block -> S.Set Register
blockAccessedRegisters (Block (_, is, i)) =
  foldl S.union (accessedRegisters i) $ map accessedRegisters is

-- Removes unused register assignments in the given blocks.
removeUnusedAssignments :: [Block] -> [Block]
removeUnusedAssignments bs =
  let rs = foldl S.union S.empty $ map blockAccessedRegisters bs in
    map (flip updateBlock rs) bs
  where
    updateBlock :: Block -> S.Set Register -> Block
    updateBlock (Block (l, is, i)) rs =
      Block (l, filter (flip isUsed rs) $ map (flip updateInstruction rs) is, i)
    updateInstruction :: Instruction -> S.Set Register -> Instruction
    updateInstruction i@(ICall rt f args (Just r)) rs =
      if r `S.member` rs then i else ICall rt f args Nothing
    updateInstruction i _ = i
    isUsed :: Instruction -> S.Set Register -> Bool
    isUsed i rs = case i of
      IArithm _ _ _ r -> r `S.member` rs
      ILoad _ _ r -> r `S.member` rs
      IAlloca _ r -> r `S.member` rs
      IIcmp _ _ _ _ r -> r `S.member` rs
      IPhi _ _ r -> r `S.member` rs
      IBitcast _ _ _ r -> r `S.member` rs
      IGetElementPtr _ _ _ r -> r `S.member` rs
      _ -> True

-- Removes unused register assignments from functions in the given program.
runRemoveUnusedAssignments :: Program -> Program
runRemoveUnusedAssignments (Program (ds, cs, fs)) = Program (ds, cs, map optimize fs) where
  optimize :: Function -> Function
  optimize (Function (rt, f, args, bs)) = Function (rt, f, args, removeUnusedAssignments bs)
