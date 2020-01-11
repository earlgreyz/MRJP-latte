module Analyzer.Main (runAnalyzer) where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.Error
import Analyzer.Internal
import Analyzer.TopDefinition

-- Runs analyzer with starting environment.
runAnalyzer :: (Program ErrPos) -> ExceptT String IO ()
runAnalyzer p = flip evalStateT False $ flip runReaderT (functions, M.empty) $ analyze p

analyze :: (Program ErrPos) -> Analyzer ()
analyze (Program _ ts) = do
  (vs, cs) <- defineManyTopDef ts
  case M.lookup mainIdent vs of
    Nothing -> throwError $ missingMainError
    Just (_, t) -> unless (t == mainType) $ throwError $ invalidMainTypeError t
  local (const (vs, cs)) $ mapM_ analyzeTopDef ts
