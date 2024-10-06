import Test.HUnit

import Control.Monad
import Data.HashMap.Strict

import Syntax.Abs
import Semantics.Memory
import Semantics.Delay
import Semantics.BigStep as Big
import Semantics.Denotational as Den

data SemTestCase = SemTestCase
  { program :: Com
  , testEnv :: Mem
  , denotationSteps :: Integer -- ^ Number of Later steps
  , denotationRes :: Mem
  , bigStepRes :: Mem
  } deriving (Show)

isDelayedBy :: Show a => Delay a -> Integer -> IO a
isDelayedBy (Now a) n =
  if n > 0
  then assertFailure $ "Denotation "  ++ show a ++ " reached "++ show n ++ " steps too early."
  else return a
isDelayedBy (Later d) n =
  if n > 0
  then isDelayedBy d (n - 1)
  else assertFailure $
       "Denotation not reached in time. Got "
       ++ (show $ pruneDelay 10 show "..." d) ++ " remaining."

-- This is possibly non-terminating, as it searches for Now, which may never appear.
isDelayedByAtLeast :: Show a => Delay a -> Integer -> IO a
isDelayedByAtLeast (Now a) n =
  if n > 0
  then assertFailure $ "Denotation "  ++ show a ++ " reached "++ show n ++ " steps too early."
  else return a
isDelayedByAtLeast (Later d) n = isDelayedBy d (n - 1)

testSubmapOf :: String -> Mem -> Mem -> Assertion
testSubmapOf msg m1 m2 =
  (m1 `isSubmapOf` m2) @? (msg ++ ": Expect submap of " ++ show m2 ++ " but got " ++ show m1)

mkTest :: SemTestCase -> Test
mkTest tc = TestList
  [ TestCase $ do
      m <- (Den.evalCom (program tc) (testEnv tc)) `isDelayedByAtLeast` (denotationSteps tc)
      testSubmapOf "Denotation" (denotationRes tc) m
  , TestCase $ testSubmapOf "Big-step" (bigStepRes tc) (Big.evalCom (program tc) (testEnv tc))
  ]

x, y, z, n :: Ident
x = Ident "x"
y = Ident "y"
z = Ident "z"
n = Ident "n"

skipTest = mkTest $ SemTestCase CSkip emptyMem 0 emptyMem emptyMem

addOneTest = mkTest $ SemTestCase c m1 0 m2 m2
  where
    c = CAssign x (EPlus (EInt 1) (EVar x))
    m1 = mkMem $ Env [Assign x 0]
    m2 = mkMem $ Env [Assign x 1]

noStepWhileTest = mkTest $ SemTestCase c m1 0 m2 m2
  where
    c = CWhile (EEq (EVar x) (EInt 1))
        (CAssign x (EPlus (EInt 1) (EVar x)))
    m1 = mkMem $ Env [Assign x 0]
    m2 = mkMem $ Env [Assign x 0]

countFourTest = mkTest $ SemTestCase c m1 4 m2 m2
  where
    c = CWhile (ELeq (EVar x) (EInt 3))
        (CAssign x (EPlus (EInt 1) (EVar x)))
    m1 = mkMem $ Env [Assign x 0]
    m2 = mkMem $ Env [Assign x 4]

seqAddOneTest = mkTest $ SemTestCase c m1 0 m2 m2
  where
    c = CSeq (CAssign x (EInt 1)) (CAssign x (EPlus (EVar x) (EInt 1)))
    m1 = mkMem $ Env [Assign x 0]
    m2 = mkMem $ Env [Assign x 2]

fib :: Com
fib =
  CSeq (CAssign x (EInt 0))
  (CSeq (CAssign y (EInt 1))
   (CWhile (ELeq (EInt 0) (ESub (EVar n) (EInt 1)))
    (CSeq (CAssign z (EPlus (EVar x) (EVar y)))
     (CSeq (CAssign x (EVar y))
      (CSeq (CAssign y (EVar z))
       (CAssign n (ESub (EVar n) (EInt 1))))))))

fibTest = mkTest $ SemTestCase fib m1 4 m2 m2
  where
    m1, m2 :: Mem
    m1 = mkMem $ Env [Assign n 4]
    m2 = mkMem $ Env [Assign x 3]

omega :: Com
omega = CWhile (EVal BTrue) CSkip

nonTerminationTest :: Test
nonTerminationTest = TestCase $ do
  let r = pruneDelay 100 (const ()) () $ Den.evalCom omega emptyMem
  case r of
    Now _ -> assertFailure "Denotation of non-terminating loop terminated";
    Later _ -> return ()

tests :: Test
tests = test
  [
    "skip" ~: skipTest
  , "Cmd: x := x + 1" ~: addOneTest
  , "While-loop with false condition" ~: noStepWhileTest
  , "Cmd: while (x <= 3) (x := x + 1)" ~: countFourTest
  , "Cmd: x:=1; x:=x+1" ~: seqAddOneTest
  , "fib" ~: fibTest
  , "non-termination" ~: nonTerminationTest
  ]
  where

main :: IO ()
main = void $ runTestTT tests
