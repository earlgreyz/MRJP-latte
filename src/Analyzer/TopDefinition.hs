module Analyzer.TopDefinition where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer
import Analyzer.Assert
import Analyzer.Error
import Analyzer.Internal
import Analyzer.Statement
import Analyzer.Util

-- Convert list of arguments into a list of its types.
argTypes :: [Arg ErrPos] -> [Type ErrPos]
argTypes args = map transform args where
  transform (Arg _ t _) = t

-- Insert arguments into the environment.
insertArgs :: [Arg ErrPos] -> Analyzer Vars
insertArgs [] = askVars
insertArgs ((Arg a t arg):args) = do
  vs <- askVars
  assertDeclarableType a t arg
  case M.lookup arg vs of
    Just (True, tt) -> throwError $ redeclaredError a arg (getTypeErrPos tt)
    otherwise -> localVars (const $ M.insert arg (True, t) vs) $ insertArgs args

-- Insert class fields into the environment.
insertFields :: Fields -> Vars -> Vars
insertFields fs vs = M.union vs $ M.map (\t -> (True, t)) fs

-- Define class attribute or method.
defineField :: Fields -> (Field ErrPos) -> Analyzer Fields
defineField fs (Attr a t x) = case M.lookup x fs of
  Just tt -> throwError $ redeclaredError a x (getTypeErrPos tt)
  Nothing -> return $ M.insert x t fs
defineField fs (Method _ (FunDef a r f args _)) = case M.lookup f fs of
  Just tt -> throwError $ redeclaredError a f (getTypeErrPos tt)
  Nothing -> return $ M.insert f (Fun a r (argTypes args)) fs

-- Collect definition and return environment with added top definitons.
defineTopDef :: TopDef ErrPos -> Analyzer Env
defineTopDef (FnDef _ (FunDef a r f args _)) = do
  (vs, cs) <- ask
  case M.lookup f vs of
    Just (_, tt) -> throwError $ redeclaredError a f (getTypeErrPos tt)
    Nothing -> return ()
  let ft = Fun a r (argTypes args)
  return (M.insert f (True, ft) vs, cs)
defineTopDef (ClDef a cls fields) = do
  (vs, cs) <- ask
  assertClassNotRedeclared a cls
  fs <- foldM defineField M.empty fields
  return (vs, M.insert cls (S.singleton cls, fs) cs)
defineTopDef (ClExtDef a cls base fields) = do
  (vs, cs) <- ask
  assertClassNotRedeclared a cls
  (bs, fs) <- mustLookupClass a base
  fs' <- foldM defineField fs fields
  let bs' = S.insert cls bs
  return (vs, M.insert cls (bs', fs') cs)

-- Iterate through a list of top definitions and insert them into the environment.
defineManyTopDef :: [TopDef ErrPos] -> Analyzer Env
defineManyTopDef ts =
  ask >>= \start -> foldlM (\env t -> local (const env) $ defineTopDef t) start ts

-- Runs static analysis on function definition.
analyzeFunction :: FunDef ErrPos -> Analyzer ()
analyzeFunction (FunDef a t f args block) = do
  modify $ \_ -> False
  vs <- localVars (\vs -> startBlock vs) $ insertArgs args
  localVars (const $ insertRet t vs) $ analyzeBlock block
  ret <- get
  unless (t == Void Nothing || ret) $ throwError $ missingReturnError a f

-- Runs static analysis on class field.
analyzeField :: Ident -> (Field ErrPos) -> Analyzer ()
analyzeField _ (Attr a t x) = assertDeclarableType a t x
analyzeField cls (Method a f) = do
  (vs, cs) <- ask
  let vs' = M.insert selfIdent (True, Class a cls) vs
  let (_, fs) = cs M.! cls
  localVars (const $ insertFields fs vs') $ analyzeFunction f

-- Runs static analysis on a top definition.
analyzeTopDef :: (TopDef ErrPos) -> Analyzer ()
analyzeTopDef (FnDef _ f) = analyzeFunction f
analyzeTopDef (ClDef a cls fields) = mapM_ (analyzeField cls) fields
analyzeTopDef (ClExtDef a cls _ fields) = mapM_ (analyzeField cls) fields
