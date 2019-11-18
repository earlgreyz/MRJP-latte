module Analyzer.Analyzer where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader

import Latte.AbsLatte

type Env = M.Map Ident (Type ErrPos)
type Analyzer = ReaderT Env (ExceptT String IO)

throwAnalyzerError :: ErrPos -> String -> Analyzer ()
throwAnalyzerError e s = throwError $ (showErrorPos e) ++ s

runAnalyzer :: (Program ErrPos) -> ExceptT String IO ()
runAnalyzer p = runReaderT (analyze p) M.empty

analyze :: (Program ErrPos) -> Analyzer ()
analyze (Program e d) = do
  throwAnalyzerError e "Invalid program"
