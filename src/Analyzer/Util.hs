module Analyzer.Util where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Except
import Control.Monad.Reader

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.Error

mustLookup :: ErrPos -> Ident -> Analyzer (Type ErrPos)
mustLookup a x = askVars >>= \vs -> case M.lookup x vs of
  Nothing -> throwError $ undefinedError a x
  Just (_, t) -> return t

mustLookupClass :: ErrPos -> Ident -> Analyzer (S.Set Ident, Fields)
mustLookupClass a cls = askClasses >>= \cs -> case M.lookup cls cs of
  Nothing -> throwError $ undefinedClassError a cls
  Just (bs, fs) -> return (bs, fs)

mustLookupField :: ErrPos -> Ident -> Ident -> Analyzer (Type ErrPos)
mustLookupField a cls f = mustLookupClass a cls >>= \(_, fs) -> case M.lookup f fs of
  Nothing -> throwError $ undefinedAttributeError a cls f
  Just t -> return t
