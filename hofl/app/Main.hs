module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import Data.Tree (drawTree)

import Syntax.Lex
import Syntax.Par
import Syntax.PrintClass
import Syntax.Abs as Raw
import Syntax.Tree
import DeBruijn.Abs as DB
import DeBruijn.Translate
import Semantics.Delay
import Semantics.BigStep as Big
import Semantics.Denotational as Den


import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Raw.Exp -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Raw.Exp -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree
                          e <- return $ toDeBruijnTree tree
                          putStrV v $ "\n[De Bruijn tree]\n\n" ++ (drawTree $ toTree e)
                          putStrV v $ "\n[Abstract De Bruijn Syntax]\n\n" ++ show e

                          exitSuccess

parse :: (Show a) => Verbosity -> ParseFun a -> String -> IO a
parse v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          return tree


evalFile :: Verbosity -> ParseFun Raw.Exp -> String -> Environment -> FilePath -> IO ()
evalFile v pExp sem env f = putStrLn f >> readFile f >>= eval v pExp sem env

eval :: Verbosity -> ParseFun Raw.Exp -> String -> Raw.Environment -> String -> IO ()
eval v pExp sem env prog = do
  e <- parse v (fmap toDeBruijnTree . pExp) prog
  let r = case sem of
            "b"  -> Ok . show . Big.evalExp e $ Big.mkEnv env
            "d"  -> Ok . show . pruneDelay 100 show "..." . Den.evalExp e $ Den.mkEnv env
            "u"  -> Ok . show . unsafeFromNow . Den.evalExp e $ Den.mkEnv env
            _    -> Bad $ "Unknown semantics \"" ++ sem ++ "\" - should be b or d"
    in case r of
    Bad s -> do
        putStrLn "Evaluation failed"
        putStrLn s
    Ok s -> putStrLn $ "Evaluation result:" ++ s

showTree :: (Show a, Print a, Treeish a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Syntax tree]\n\n"     ++ (drawTree $ toTree tree)
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    , "  -e (semantics) (environment) (files)   Parse content of files and evaluate with given "
      ++ "semantics (b - big-step, d - denotational, u - denotational with unsafe print) and environment"
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pExp
    "-s":fs -> mapM_ (runFile 0 pExp) fs
    "-e":s:envS:fs -> parse 2 pEnvironment envS >>= \env -> mapM_ (evalFile 2 pExp s env) fs
    fs -> mapM_ (runFile 2 pExp) fs
