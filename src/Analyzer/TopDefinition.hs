module Analyzer.TopDefinition where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.Error
import Analyzer.Statement

-- Convert list of arguments into a list of its types.
argTypes :: [Arg ErrPos] -> [Type ErrPos]
argTypes args = map transform args where
  transform (Arg _ t _) = t

-- Insert arguments into the environment.
insertArgs :: [Arg ErrPos] -> Analyzer Env
insertArgs [] = ask
insertArgs ((Arg a t arg):args) = do
  env <- ask
  case M.lookup arg env of
    Just (True, tt) -> throwError $ redeclaredError a arg (getTypeErrPos tt)
    _ -> local (\_ -> M.insert arg (True, t) env) $ insertArgs args

-- Iterate through a list of top definitions and insert them into the environment.
defineManyTopDef :: [TopDef ErrPos] -> Analyzer Env
defineManyTopDef [] = ask
defineManyTopDef ((FnDef a r f args _):ds) = do
  env <- ask
  case M.lookup f env of
    Just (_, tt) -> throwError $ redeclaredError a f (getTypeErrPos tt)
    Nothing -> return ()
  let ft = Fun a r (argTypes args)
  local (\env -> M.insert f (True, ft) env) $ defineManyTopDef ds

analyzeTopDef :: (TopDef ErrPos) -> Analyzer ()
analyzeTopDef (FnDef a t f args block) = do
  modify $ \_ -> False
  env <- local (\env -> startBlock env) $ insertArgs args
  local (\_ -> insertRet t env) $ analyzeBlock block
  ret <- get
  unless (t == Void Nothing || ret) $ throwError $ missingReturnError a f
