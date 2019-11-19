module Analyzer.Analyzer where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Error

type AnalyzerType = (Bool, Type ErrPos)
type Env = M.Map Ident AnalyzerType
type Analyzer = ReaderT Env (ExceptT String IO)
