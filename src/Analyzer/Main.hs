module Analyzer.Main where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.TopDefinition

-- Runs analyzer with starting environment.
runAnalyzer :: (Program ErrPos) -> ExceptT String IO ()
runAnalyzer p = runReaderT (analyze p) M.empty

analyze :: (Program ErrPos) -> Analyzer ()
analyze (Program _ ts) = do
  env <- defineManyTopDef ts
  local (\_ -> env) $ mapM_ analyzeTopDef ts
