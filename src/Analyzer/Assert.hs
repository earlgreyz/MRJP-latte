module Analyzer.Assert where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.Error

-- Assert identifier has not been declared in the current block.
assertNotRedeclared :: ErrPos -> Ident -> Analyzer ()
assertNotRedeclared a x = do
  env <- ask
  case M.lookup x env of
    Just (True, tt) -> throwError $ redeclaredError a x (getTypeErrPos tt)
    _ -> return ()

-- Assert type matches the required type.
assertType :: ErrPos -> Type ErrPos -> Type ErrPos -> Analyzer ()
assertType a t tt = do
  unless (t == tt) $ throwError $ typeExpectedError a t tt

-- Assert type matches one of the required type.
assertOneOfType :: ErrPos -> [Type ErrPos] -> Type ErrPos -> Analyzer ()
assertOneOfType a ts t = do
  unless (any ((==) t) ts) $ throwError $ oneOfTypeExpectedError a ts t
