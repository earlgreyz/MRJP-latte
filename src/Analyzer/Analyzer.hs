module Analyzer.Analyzer where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Error

-- Flag if has been defined in current block and type.
type AnalyzerType = (Bool, Type ErrPos)
-- Mapping of variables to types.
type Vars = M.Map Ident AnalyzerType
-- Mapping of class fields to types.
type Fields = M.Map Ident (Type ErrPos)
-- Mapping of class names to its parent classes and field definitions
type Classes = M.Map Ident (S.Set Ident, Fields)

type Env = (Vars, Classes)

type IExcept = ExceptT String IO
type IState = StateT Bool IExcept
type Analyzer = ReaderT Env IState

askVars :: Analyzer Vars
askVars = ask >>= \(vs, _) -> return vs

askClasses :: Analyzer Classes
askClasses = ask >>= \(_, cs) -> return cs

localVars :: (Vars -> Vars) -> Analyzer a -> Analyzer a
localVars f = local (\(vs, cs) -> (f vs, cs))

localClasses :: (Classes -> Classes) -> Analyzer a -> Analyzer a
localClasses f = local (\(vs, cs) -> (vs, f cs))
