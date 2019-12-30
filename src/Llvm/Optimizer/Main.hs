module Llvm.Optimizer.Main (runOptimizer) where

import Util.Function

import Llvm.Llvm
import Llvm.Util

import Llvm.Optimizer.TrivialPhis
import Llvm.Optimizer.UnreachableBlocks
import Llvm.Optimizer.UnusedAssignments

runOptimizer :: Program -> Program
runOptimizer = fixPoint $ compose [
    runRemoveUnreachableBlocks,
    runRemoveTrivialPhis,
    runRemoveUnusedAssignments]
