module Llvm.Optimizer.Main (runOptimizer) where

import Llvm.Llvm
import Llvm.Util
import Llvm.Optimizer.UnreachableBlocks
import Llvm.Optimizer.UnusedAssignments

runOptimizer :: Program -> Program
runOptimizer = fixPoint $ optimizerStep
  where
    optimizerStep :: Program -> Program
    optimizerStep = compose [
      runRemoveUnreachableBlocks,
      runRemoveUnusedAssignments]
