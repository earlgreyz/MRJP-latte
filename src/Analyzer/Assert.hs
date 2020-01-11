module Analyzer.Assert where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.Error
import Analyzer.Util

-- Assert identifier has not been declared in the current block.
assertNotRedeclared :: ErrPos -> Ident -> Analyzer ()
assertNotRedeclared a x = do
  vs <- askVars
  case M.lookup x vs of
    Just (True, tt) -> throwError $ redeclaredError a x (getTypeErrPos tt)
    otherwise -> return ()

-- Assert identifier is not a class definition.
assertNotClass :: ErrPos -> Ident -> Analyzer ()
assertNotClass a c = do
  cs <- askClasses
  case M.lookup c cs of
    Just _ -> throwError $ classRedeclaredError a c
    otherwise -> return ()

-- Assert a variable of a given type can be defined.
assertDeclarableType :: ErrPos -> Type ErrPos -> Ident -> Analyzer ()
assertDeclarableType a t x = case t of
  Void _ -> throwError $ invalidDeclarationError a x t
  Class _ cls -> mustLookupClass a cls >> return ()
  Array _ t -> assertDeclarableType a t x
  Fun _ _ _ -> throwError $ invalidDeclarationError a x t
  otherwise -> return ()

-- Assert type matches the required type.
assertType :: ErrPos -> Type ErrPos -> Type ErrPos -> Analyzer ()
assertType a t tt = do
  unless (t == tt) $ throwError $ typeExpectedError a t tt

-- Assert type matches one of the required type.
assertOneOfType :: ErrPos -> [Type ErrPos] -> Type ErrPos -> Analyzer ()
assertOneOfType a ts t = do
  unless (any ((==) t) ts) $ throwError $ oneOfTypeExpectedError a ts t
