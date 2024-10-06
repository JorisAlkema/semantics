import Test.HUnit

import Control.Monad
import Data.HashMap.Strict

import Syntax.Abs as Raw
import qualified DeBruijn.Abs as DB
import DeBruijn.Translate
import Semantics.Delay
import qualified Semantics.BigStep as Big
import qualified Semantics.Denotational as Den
import Semantics.Denotational (dInt, dBool)

import Programs

-- | Type of messages the test predicates can accumulate and print.
type Message = String

-- | As we cannot compare elements of Den.Value for equality, we allow more general tests.
-- For instance, we can apply a function f given by VMap f to arguments and test the outcome.
-- This type encapsulates test predicates that also receive a message that can be accumulated
-- and printed on a failed assertion.
type DenTester = Den.Value -> Message -> Assertion

-- | Runs predicate with initially empty message
runT :: DenTester -> Den.Value -> Assertion
runT p = flip p ""

-- | Extend a message
putM :: Message -> String -> Message
putM m m' = m ++ " -> " ++ m'

-- | Constructor for basic equality test
eqTest :: Den.Value -> DenTester
eqTest expected given message = assertEqual message expected given

-- | Specialisation of eqTest to integer values.
intTest :: Integer -> DenTester
intTest = eqTest . Den.VInt

-- | Specialisation of eqTest to boolean values.
boolTest :: Bool -> DenTester
boolTest = eqTest . Den.VBool

-- | Tests if a value is of the form VMap f, and then checks whether f arg terminates in n steps
-- and f arg == res.
appTest :: Integer -> Den.Domain -> DenTester -> DenTester
appTest n arg p v m = case v of
  Den.VMap f -> do
    r <- isDelayedByAtLeast (f arg) n
    p r (putM m $ "Application of" ++ show v ++ " to " ++ show arg)
  _ -> assertFailure $ putM m $ "Cannot apply non-function " ++ show v ++ " to " ++ show arg

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
isDelayedByAtLeast (Later d) n = isDelayedByAtLeast d (n - 1)

isDelayedByD :: Den.Domain -> Integer -> IO Den.Value
isDelayedByD = isDelayedByAtLeast

isMapAfter :: Den.Domain -> Integer -> IO (Den.Domain -> Den.Domain)
isMapAfter d n = isDelayedByAtLeast d n >>= \v -> case v of
  Den.VMap f -> return f
  _          -> assertFailure $ "Expected VMap but got " ++ show v ++ "."

-- | Encapsulates test cases for the semantics of programs that can be used as HUnit Test
data SemTestCase = SemTestCase
  { program :: DB.Exp -- ^ The program to test
  , testEnv :: Raw.Environment -- ^ The environment in which the program is run
  , bigStepRes :: Big.Value -- ^ Value that is expected to be returned by the big-step semantics
  , denotationSteps :: Integer -- ^ Number of Later steps that the denotational semantics should need
  , denotationRes :: DenTester -- ^ Predicate to test the denotational semantics of the program
  }

instance Testable SemTestCase where
  test tc = TestList
    [
        "Denotation" ~: TestCase $ do
          m <- (Den.evalExp (program tc) (Den.mkEnv $ testEnv tc)) `isDelayedByD` (denotationSteps tc)
          runT (denotationRes tc) m
      ,
      "Big-step" ~: (bigStepRes tc ~=? Big.evalExp (program tc) (Big.mkEnv $ testEnv tc))
    ]

idTest = SemTestCase idE (Env []) (Big.VThunk idE) 0 (appTest 0 (dInt 1) (intTest 1))

envTest = SemTestCase (idE $$ n) env (Big.VInt 10) 0 (intTest 10)
  where
    env :: Raw.Environment
    env = Env [Assign (Ident "n") (Val 10)]

projTest pr v res =
  SemTestCase e (Env []) (Big.VBool res) 0 (boolTest res)
  where
    e = pr triple === DB.Val v

factTest =
  SemTestCase factT (Env []) (Big.VThunk factRoll) 0 (appTest 4 (return $ Den.VInt 4) (intTest 24))

fibTest = SemTestCase fib env (Big.VInt 55) (2^8 - 79) (intTest 55)
  where
    env :: Raw.Environment
    env = Env [Assign (Ident "n") (Val 10)]

ackermannTest m n r steps =
  SemTestCase ackermann env (Big.VInt r) steps (intTest r)
  where
    env :: Raw.Environment
    env = Env [Assign (Ident "m") (Val m), Assign (Ident "n") (Val n)]

forsomeTest :: Raw.Exp -> Bool -> Integer -> SemTestCase
forsomeTest p res steps =
  SemTestCase forsome env (Big.VBool res) steps (boolTest res)
  where
    env :: Raw.Environment
    env = Env [Assign (Ident "p") p]

p1, p2, p3, p4 :: Raw.Exp
p1 = Abstr (Lam "\\") (Ident "s") (App (Var (Ident "s")) (Val 1))
p2 = Abstr (Lam "\\") (Ident "s") (BVal BFalse)
p3 = Abstr (Lam "\\") (Ident "s") (BNeg (Neg "~") (App (Var (Ident "s")) (Val 1)))
p4 = Abstr (Lam "\\") (Ident "s")
     (BOr
      (BAnd
       (App (Var (Ident "s")) (Val 0))
       (And "&&")
       (App (Var (Ident "s")) (Val 1)))
      (Or "||")
      (App (Var (Ident "s")) (Val 3)))


tests :: Test
tests = test
  [ "id" ~: idTest
  , "fst (1, 2, 3) == 1" ~: projTest DB.Fst 1 True
  , "fst (snd (1, 2, 3)) == 2" ~: projTest (DB.Fst . DB.Snd) 2 True
  , "snd (snd (1, 2, 3)) == 3" ~: projTest (DB.Snd . DB.Snd) 3 True
  , "~(snd (1, 2, 3) == 3)" ~: projTest (DB.Fst . DB.Snd) 3 False
  , "fact" ~: factTest
  , "fib" ~: fibTest
  --
  , "ackermann(0, 0)" ~: (ackermannTest 0 0 1 1)  -- = 0 + 1 = 1
  , "ackermann(0, 1)" ~: (ackermannTest 0 1 2 1)  -- = 1 + 1 = 2
  , "ackermann(0, 2)" ~: (ackermannTest 0 2 3 1)  -- = 2 + 1 = 3
  , "ackermann(0, 3)" ~: (ackermannTest 0 3 4 1)  -- = 3 + 1 = 4
  , "ackermann(0, 4)" ~: (ackermannTest 0 4 5 1)  -- = 4 + 1 = 5
  , "ackermann(1, 0)" ~: (ackermannTest 1 0 2 2)  -- = A(0, 1) = 2
  , "ackermann(1, 1)" ~: (ackermannTest 1 1 3 4)  -- = A(0, A(1, 0)) = A(0, 2) = 3
  , "ackermann(2, 0)" ~: (ackermannTest 2 0 3 5)  -- = A(1, 1) = 3
  , "ackermann(1, 2)" ~: (ackermannTest 1 2 4 6)  -- = A(0, A(1, 1)) = A(0, 3) = 4
  , "ackermann(1, 3)" ~: (ackermannTest 1 3 5 8)  -- = A(0, A(1, 2)) = A(0, 4) = 5
  , "ackermann(2, 1)" ~: (ackermannTest 2 1 5 29)  -- = A(1, A(2, 0)) = A(1, 3) = 5
  , "ackermann(3, 0)" ~: (ackermannTest 3 0 5 30)  -- = A(2,1) = 5
  --
  , "forsome (\\s.s 1)" ~: forsomeTest p1 True 3
  , "forsome (\\s.false)" ~: forsomeTest p2 False 0
  , "forsome (\\s.~(s 1))" ~: forsomeTest p3 True 3
  , "forsome (\\s.(s 0 && s 1) || s 3)" ~: forsomeTest p4 True 47
  ]
  where

main :: IO ()
main = void $ runTestTT tests
