{-# LANGUAGE FlexibleContexts #-}

module Semantics.Denotational where

import Debug.Trace

import Control.Monad.Reader
import Control.Applicative

import Data.HashMap.Lazy as HM

import qualified Syntax.Abs as Raw
import Syntax.PrintClass
import DeBruijn.Abs as DB
import DeBruijn.Translate
import DeBruijn.Substitution
import ErrM
import Semantics.Delay

data Value
  = VInt Integer
  | VBool Bool
  | VPair Domain Domain
  | VMap (Domain -> Domain)

type Domain = Delay Value

dInt :: Integer -> Domain
dInt = pure . VInt

dBool :: Bool -> Domain
dBool = pure . VBool

data Env = Env
  { free :: HashMap String DB.Exp
  , bound :: [Domain]
  } deriving (Show)

type R a = Env -> a

instance Eq Value where
  VInt  m   == VInt  n   = m == n
  VBool a   == VBool b   = a == b
  VPair a b == VPair x y = a == x && b == y
  _         == _         = False

instance Ord Value where
  VInt  m   <= VInt  n   = m <= n
  VBool a   <= VBool b   = a <= b
  VPair a b <= VPair x y = a <= x && b <= y
  _         <= _         = False

instance Show Value where
  show (VInt i) = "VInt " ++ show i
  show (VBool i) = "VBool " ++ show i
  show (VPair d1 d2) = "VPair (" ++ show d1 ++ ") (" ++ show d2 ++ ")"
  show (VMap _) = "VMap"

emptyEnv :: Env
emptyEnv = Env HM.empty []

mkEnv :: Raw.Environment -> Env
mkEnv (Raw.Env e) = Env (fromList $ fmap mkAssign e) []
  where
    mkAssign (Raw.Assign (Raw.Ident x) exp) = (x, toDeBruijnTree exp)

updateFree :: String -> DB.Exp -> Env -> Env
updateFree x v e = e { free = insert x v (free e) }

-- -- | Binds a value to a variable
bindValue :: Domain -> Env -> Env
bindValue v e = e { bound = v : bound e }
-- bindValue v e = trace ("Updating bound with " ++ show v) $ e { bound = v : bound e }

getFree :: String -> Env -> DB.Exp
getFree x e = free e ! x

getBound :: Int -> Env -> Domain
getBound i e = bound e !! i
-- getBound i e = trace ("Retrieving <" ++ show i ++ "> in " ++ show (bound e)) $ bound e !! i

evalBConst :: BConst -> R Domain
evalBConst v _ = dBool  $ case v of
  BTrue  -> True
  BFalse -> False

evalExp :: Exp -> Env -> Domain
evalExp = denExp

thunkMap :: (Domain -> R Domain) -> R Domain
thunkMap f env = pure $ VMap $ \d -> f d env

denExp :: Exp -> R Domain
denExp exp = case exp of
 Var (Bound i _) -> getBound i
  Var (Free x)    -> \env -> denExp (getFree x env) env
  Val v           -> \env -> dInt v
  BVal b          -> evalBConst b
  App e1 e2       -> \env ->
    denExp e1 env >>= \v1 ->
    denExp e2 env >>= \v2 ->
    case v1 of
      VMap f -> f v2
      _      -> error $ "Expected function in app, got: " ++ show v1
  Fst e           -> \env ->
    denExp e env >>= \v ->
    case v of
      VPair d1 _ -> d1
      _          -> error $ "expected a pair in fst, got: " ++ show v
  Snd e           -> \env ->
    denExp e env >>= \v ->
    case v of
      VPair _ d2 -> d2
      _          -> error $ "Expected a pair in snd, got: " ++ show v
  Mul e1 e2       -> evalInt2 (*) e1 e2
  Add e1 e2       -> evalInt2 (+) e1 e2
  Sub e1 e2       -> evalInt2 (-) e1 e2
  Pair e1 e2      -> \env ->
    denExp e1 env >>= \d1 ->
    denExp e2 env >>= \d2 ->
    pure $ VPair d1 d2
  BEq e1 e2       -> \env ->
    denExp e1 env >>= \d1 ->
    denExp e2 env >>= \d2 ->
    pure $ dBool (unsafeFromNow d1 == unsafeFromNow d2)
  BLeq e1 e2      -> \env ->
    denExp e1 env >>= \d1 ->
    denExp e2 env >>= \d2 ->
    case (unsafeFromNow d1, unsafeFromNow d2) of
      (VInt n1, VInt n2) -> pure $ dBool (n1 <= n2)
      _ -> error $ "Expected integer in BLeq, but got: " ++ show (unsafeFromNow d1) ++ " and " ++ show (unsafeFromNow d2)
  BNeg _  b1      -> evalBool1 not b1
  BAnd b1 _ b2    -> evalBool2 (&&) b1 b2
  BOr  b1 _ b2    -> evalBool2 (||) b1 b2
  Ite bexp e1 e2  -> \env ->
    denExp bexp env >>= \cond ->
    case unsafeFromNow cond of
      VBool True  -> denExp e1 env
      VBool False -> denExp e2 env
      _           -> error $ "Expected Boolean in Ite, but got: " ++ show cond
  Abstr _ e       -> thunkMap (\d -> bindValue d (Env (free env) (bound env)) >>= denExp e)
  Rec x e         -> \env ->
    let recursiveEnv = Env (free env) (bindValue (delayRec x e env) env)
    in denExp e recursiveEnv
  Typed e ty      -> denExp e

-- | Helper function to create a recursive delay
delayRec :: String -> Exp -> Env -> Domain
delayRec x e env = bot -- Placeholder; proper recursion is handled differently

-- | Helper function that combines the outcome of calling denExp on two expressions
-- with a binary integer operation.
evalInt2 :: (Integer -> Integer -> Integer) -> Exp -> Exp -> R Domain
evalInt2 f e1 e2 env = do
  r1 <- denExp e1 env
  r2 <- denExp e2 env
  case (r1, r2) of
    (VInt n1, VInt n2) -> pure $ VInt $ f n1 n2
    _ -> error $ "Expected integer values but got " ++ show r1 ++ " and " ++ show r2

-- | Helper function that combines the outcome of calling denExp on one expression
-- with a unary Boolean operation.
evalBool1 :: (Bool -> Bool) -> Exp -> R Domain
evalBool1 f e env = do
  r <- denExp e env
  case r of
    VBool b -> pure $ VBool $ f b
    _ -> error $ "Expected Boolean values but got " ++ show r

-- | Helper function that combines the outcome of calling denExp on two expressions
-- with a binary Boolean operation.
evalBool2 :: (Bool -> Bool -> Bool) -> Exp -> Exp -> R Domain
evalBool2 f e1 e2 env = do
  r1 <- denExp e1 env
  r2 <- denExp e2 env
  case (r1, r2) of
    (VBool b1, VBool b2) -> pure $ VBool $ f b1 b2
    _ -> error $ "Expected Boolean values but got " ++ show r1 ++ " and " ++ show r2
