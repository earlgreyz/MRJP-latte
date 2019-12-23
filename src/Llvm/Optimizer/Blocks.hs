module Llvm.Optimizer.Blocks (runRemoveUnreachableBlocks) where

import qualified Data.Map as M
import qualified Data.Set as S

import Llvm.Llvm

-- Creates neighbours map from blocks.
neighbours :: [Block] -> M.Map Label [Label]
neighbours bs = M.fromList $ flip map bs $ \b -> (blockLabel b, blockNeighbours b) where
  blockNeighbours :: Block -> [Label]
  blockNeighbours (Block (_, _, i)) = case i of
    IBr l -> [l]
    IBrCond _ l r -> [l, r]
    _ -> []

-- Runs DFS to determine blocks reachable from the starting label.
reachable :: Label -> M.Map Label [Label] -> S.Set Label
reachable l neighbours = visit [l] neighbours S.empty where
  visit :: [Label] -> M.Map Label [Label] -> S.Set Label -> S.Set Label
  visit (l:ls) neighbours visited =
    if l `S.member` visited then
      visit ls neighbours visited
    else let ns = M.findWithDefault [] l neighbours in
      visit ls neighbours (visit ns neighbours $ l `S.insert` visited)
  visit [] _ visited = visited

-- Removes blocks unreachable from the starting block.
removeUnreachableBlocks :: [Block] -> [Block]
removeUnreachableBlocks bs =
  let ns = neighbours bs in
    let rs = reachable (blockLabel $ head bs) ns in
      map (flip updateBlock rs) $ filter (\b -> blockLabel b `S.member` rs) bs
  where
    updateBlock :: Block -> S.Set Label -> Block
    updateBlock (Block (l, is, i)) rs =
      Block (l, map (flip updateInstruction rs) is, i)
    updateInstruction :: Instruction -> S.Set Label -> Instruction
    updateInstruction (IPhi t args res) rs =
      IPhi t (filter (\(_, l) -> l `S.member` rs) args) res
    updateInstruction i _ = i

-- Removes unreachable blocks from functions in the given program.
runRemoveUnreachableBlocks :: Program -> Program
runRemoveUnreachableBlocks (Program (ds, cs, fs)) = Program (ds, cs, map optimize fs) where
  optimize :: Function -> Function
  optimize (Function (rt, f, args, bs)) = Function (rt, f, args, removeUnreachableBlocks bs)
