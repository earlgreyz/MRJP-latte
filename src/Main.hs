module Main where

import System.IO ( stdin, stderr, hGetContents, openFile, hPutStr)
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad ( when )
import Control.Monad.Except

import Latte.LexLatte
import Latte.ParLatte
import Latte.SkelLatte
import Latte.PrintLatte
import Latte.AbsLatte
import Latte.ErrM
import Latte.ErrLatte

import Analyzer.Main
import Llvm.Main

myLLexer = myLexer

type ParseFun = [Token] -> Err (Program ErrPos)

runFile :: ParseFun -> FilePath -> IO ()
runFile p f = readFile f >>= run p f

run :: ParseFun -> FilePath -> String -> IO ()
run p f s = let ts = myLLexer s in case p ts of
  Bad err -> do
    hPutStr stderr "ERROR\nParse failed: "
    hPutStr stderr err
    exitFailure
  Ok program -> do
    result <- runExceptT $ runAnalyzer program
    case result of
      Left err -> do
        hPutStr stderr "ERROR\nStatic analysis failed\n"
        hPutStr stderr err
        exitFailure
      Right _ -> do
        hPutStr stderr "OK\n"
        let p = runCompileProgram program
        putStr $ show p

main :: IO ()
main = do
  args <- getArgs
  case args of
    (f:[]) -> runFile pProgram f
    _ -> do
      prog <- getProgName
      putStrLn $ "Usage: " ++ prog ++ " filename"
