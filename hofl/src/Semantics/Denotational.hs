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
denExp exp env = case exp of
  Var (Bound i _) -> getBound i env
  Var (Free x)    -> denExp (getFree x env) env
  Val v           -> dInt v
  BVal b          -> evalBConst b env

  App e1 e2 -> \env -> do
  cond  









 #KANKER FOUT
  App e1 e2 ->
    let vf = denExp e1 env
        va = denExp e2 env
    in case vf of
         Now (VMap g) -> g va
         Now _        -> error "Expected function in application"
         Later _      -> Later (denExp (App e1 e2) env)

  Fst e ->
    let vp = denExp e env
    in case vp of
         Now (VPair d1 d2) -> d1
         Now _             -> error "Expected a pair in Fst"
         Later _           -> error "Non-terminating in Fst"

  Snd e ->
    let vp = denExp e env
    in case vp of
         Now (VPair d1 d2) -> d2
         Now _             -> error "Expected a pair in Snd"
         Later _           -> error "Non-terminating in Snd"

  Mul e1 e2 -> evalInt2 (*) e1 e2 env
  Add e1 e2 -> evalInt2 (+) e1 e2 env
  Sub e1 e2 -> evalInt2 (-) e1 e2 env

  Pair e1 e2 ->
    let d1 = denExp e1 env
        d2 = denExp e2 env
    in Now (VPair d1 d2)

  BEq e1 e2 ->
    let d1 = denExp e1 env
        d2 = denExp e2 env
    in Now (VBool (d1 == d2))

  BLeq e1 e2 ->
    let d1 = denExp e1 env
        d2 = denExp e2 env
    in Now (VBool (d1 <= d2))

  BNeg _ b1      -> evalBool1 not b1 env
  BAnd b1 _ b2    -> evalBool2 (&&) b1 b2 env
  BOr  b1 _ b2     -> evalBool2 (||) b1 b2 env

  Ite bexp e1 e2 ->
    let vb = denExp bexp env
    in case vb of
         Now (VBool True)  -> Later (denExp e1 env)
         Now (VBool False) -> Later (denExp e2 env)
         Now _             -> error "Expected Boolean in if-else"
         Later _           -> Later (denExp (Ite bexp e1 e2) env)

  Abstr lam e ->
    Now (VMap (\d -> denExp e (bindValue d env)))

  Rec e ->
    mfix (\v -> denExp e (bindValue (Now v) env))

  Typed e ty ->
    denExp e env


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
