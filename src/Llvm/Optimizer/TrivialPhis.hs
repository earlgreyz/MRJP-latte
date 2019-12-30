module Llvm.Optimizer.TrivialPhis (runRemoveTrivialPhis) where

import Data.Maybe
import qualified Data.Map as M

import Llvm.Llvm
import Llvm.Util

-- Finds mapping from trivial phi register results to actual values.
getMapping :: [Block] -> M.Map Register Value
getMapping bs = M.fromList $ mapMaybe mapping $ concatMap blockInstructions bs
  where
    mapping :: Instruction -> Maybe (Register, Value)
    mapping (IPhi _ ((v, _):[]) r) = Just (r, v)
    mapping _ = Nothing

-- Determines if a phi is trivial.
isTrivialPhi :: Instruction -> Bool
isTrivialPhi (IPhi _ (_:[]) _) = True
isTrivialPhi _ = False

-- Removes trivial phis from the function blocks.
removeTrivialPhis :: [Block] -> [Block]
removeTrivialPhis bs =
  let rs = getMapping bs in
    map (flip updateBlock rs) bs
  where
    updateBlock :: Block -> M.Map Register Value -> Block
    updateBlock (Block (l, is, i)) rs =
      Block (l, map (flip updateInstruction rs) $ filter (not . isTrivialPhi) is, i)
    updateInstruction :: Instruction -> M.Map Register Value -> Instruction
    updateInstruction i rs = fmapInstructionValue i $ flip replaceMapping rs
    -- Replace mapping replaces given register value with a mapped value if found.
    replaceMapping :: Value -> M.Map Register Value -> Value
    replaceMapping v@(VReg r) rs = case r `M.lookup` rs of
      Nothing -> v
      Just s -> s
    replaceMapping v _ = v

-- Removes trivial phis from functions in the given program.
runRemoveTrivialPhis :: Program -> Program
runRemoveTrivialPhis (Program (ds, cs, fs)) = Program (ds, cs, map optimize fs) where
  optimize :: Function -> Function
  optimize (Function (rt, f, args, bs)) = Function (rt, f, args, removeTrivialPhis bs)
