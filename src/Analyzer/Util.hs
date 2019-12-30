module Analyzer.Util where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.Error

mustLookup :: ErrPos -> Ident -> Analyzer (Type ErrPos)
mustLookup a x = ask >>= \env -> case M.lookup x env of
  Nothing -> throwError $ undefinedError a x
  Just (_, t) -> return t
