module Llvm.Optimizer.Main (runOptimizer) where

import Llvm.Llvm
import Llvm.Optimizer.UnreachableBlocks
import Llvm.Optimizer.UnusedAssignments

runOptimizer :: Program -> Program
runOptimizer = runRemoveUnreachableBlocks . runRemoveUnusedAssignments
