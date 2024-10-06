{-# LANGUAGE PatternSynonyms #-}

module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import Control.Monad (when)

import Syntax.Lex
import Syntax.Par
import Syntax.Print
import Syntax.Abs
import Semantics
import Syntax.ErrM

type ParseFun a = [Token] -> Err a

myLLexer :: String -> [Token]
myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad e    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn e
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree

parse :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO a
parse v p s = let ts = myLLexer s in case p ts of
           Bad e    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn e
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          return tree

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

evalFile :: Verbosity -> ParseFun Exp -> String -> Env -> FilePath -> IO ()
evalFile v pExpr sem env f = putStrLn f >> readFile f >>= eval v pExpr sem env


eval :: Verbosity -> ParseFun Exp -> String -> Env -> String -> IO ()
eval v pExpr sem env exprS = do
  expr <- parse v pExpr exprS
  case sem of
    "d"  -> putStrLn $ show $ denotation env expr
    "b"  -> putStrLn $ show $ bigStep env expr
    "s"  -> putStrLn $ printTree $ snd $ normaliseSmallSteps (env, expr)
    "s1" -> putStrLn $ show $ smallStep (env, expr)
    "l"  -> putStrLn $ printTree $ snd $ normaliseSmallStepsLeft (env, expr)
    "l1" -> putStrLn $ printTree $ snd $ smallStepLeftFirst (env, expr)
    _    -> putStrLn "Unknown semantics - should be d, b, s, s1, l or l1"


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help                                 Display this help message."
    , "  (no arguments)                         Parse stdin verbosely."
    , "  (files)                                Parse content of files verbosely."
    , "  -s (files)                             Silent mode. Parse content of files silently."
    , "  -e (semantics) (environment) (files)   Parse content of files and evaluate with given"
      ++ "semantics (d - denotationational, b - big-step, s - small step, s1 - one small step, "
      ++ "l - small step with left-first evaluation, l1 - one left-first step) and environment"
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pExp
    "-s":fs -> mapM_ (runFile 0 pExp) fs
    "-e":s:envS:fs -> parse 2 pEnv envS >>= \env -> mapM_ (evalFile 2 pExp s env) fs
    fs -> mapM_ (runFile 2 pExp) fs
