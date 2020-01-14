module Analyzer.Internal where

import qualified Data.Map as M

import Latte.AbsLatte
import Latte.ErrLatte

import Analyzer.Analyzer

-- Create a function header.
function :: Type () -> String -> [Type ()] -> (Ident, AnalyzerType)
function r f args = (Ident f, (True, Fun Nothing (fmap make r) $ map (fmap make) args))
  where
    make :: () -> ErrPos
    make () = Nothing

-- Built in functions.
functions :: M.Map Ident AnalyzerType
functions = M.fromList [
  function (Void ()) "printInt" [Int ()],
  function (Void ()) "printString" [Str ()],
  function (Void ()) "error" [],
  function (Int ()) "readInt" [],
  function (Str ()) "readString" []]

mainIdent :: Ident
mainIdent = Ident "main"

mainType :: Type ErrPos
mainType = let (_, (_, t)) = function (Int ()) "main" [] in t

lengthIdent :: Ident
lengthIdent = Ident "length"

selfIdent :: Ident
selfIdent = Ident "self"
