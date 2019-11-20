module Analyzer.Main where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.TopDefinition

-- Create a function header.
function :: Type () -> String -> [Type ()] -> (Ident, AnalyzerType)
function r f args = (Ident f, (True, Fun Nothing (fmap make r) $ map (fmap make) args)) where
  make :: () -> ErrPos
  make () = Nothing

-- Build in functions.
functions :: M.Map Ident AnalyzerType
functions = M.fromList [
  function (Void ()) "printInt" [Int ()],
  function (Void ()) "printString" [Str ()],
  function (Void ()) "error" [],
  function (Int ()) "readInt" [],
  function (Str ()) "readString" []]

-- Runs analyzer with starting environment.
runAnalyzer :: (Program ErrPos) -> ExceptT String IO ()
runAnalyzer p = runReaderT (analyze p) functions

analyze :: (Program ErrPos) -> Analyzer ()
analyze (Program _ ts) = do
  env <- defineManyTopDef ts
  local (\_ -> env) $ mapM_ analyzeTopDef ts
