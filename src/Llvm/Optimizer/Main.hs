module Llvm.Optimizer.Main (runOptimizer) where

import Llvm.Llvm
import Llvm.Optimizer.Blocks

runOptimizer :: Program -> Program
runOptimizer = runRemoveUnreachableBlocks
