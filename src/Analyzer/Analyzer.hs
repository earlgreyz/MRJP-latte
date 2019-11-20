module Analyzer.Analyzer where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Error

type AnalyzerType = (Bool, Type ErrPos)
type Env = M.Map Ident AnalyzerType

type IExcept = ExceptT String IO
type IState = StateT Bool IExcept
type Analyzer = ReaderT Env IState
