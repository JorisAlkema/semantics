{-# LANGUAGE FlexibleContexts #-}

module Semantics.BigStep where

import Control.Monad.Reader
import Control.Applicative
import Data.HashMap.Lazy as HM

import Syntax.PrintClass
import qualified Syntax.Abs as Raw
import DeBruijn.Abs as DB
import DeBruijn.Translate
import DeBruijn.Substitution
import ErrM

data Value
  = VInt Integer
  | VBool Bool
  | VPair Value Value
  | VThunk DB.Exp
  deriving (Eq, Show)

instance Ord Value where
  VInt m <= VInt n = m <= n
  VBool a <= VBool b = a <= b
  _ <= _ = False

toExp :: Value -> Exp
toExp (VInt n) = Val n
toExp (VBool True) = BVal BTrue
toExp (VBool False) = BVal BFalse
toExp (VThunk e) = e

type Env = HashMap String DB.Exp

emptyEnv :: Env
emptyEnv = HM.empty

mkEnv :: Raw.Environment -> Env
mkEnv (Raw.Env e) = fromList $ fmap mkAssign e
  where
    mkAssign (Raw.Assign (Raw.Ident x) exp) = (x, toDeBruijnTree exp)

update :: String -> DB.Exp -> Env -> Env
update = insert

getVal :: String -> Env -> DB.Exp
getVal x e = e ! x

evalBConst :: MonadReader Env m => BConst -> m Value
evalBConst v = case v of
  BTrue  -> pure $ VBool True
  BFalse -> pure $ VBool False

evalExp :: MonadReader Env m => Exp -> m Value
evalExp exp = case exp of
  Var (Bound _ _) -> error "How did we end up here?"
  Var (Free x)    -> asks (getVal x) >>= evalExp
  Val v           -> pure $ VInt v
  BVal b          -> evalBConst b
  App e1 e2       -> do
    v1 <- evalExp e1
    case v1 of
      VThunk (t@(Abstr _ _)) -> evalExp (removeBinder t e2)
      _ -> error $ "Expected function in application"
  Fst e           -> failure exp
  Snd e           -> failure exp
  Mul e1 e2       -> failure exp
  Add e1 e2       -> failure exp
  Sub e1 e2       -> failure exp
  Pair e1 e2      -> failure exp
  BEq  e1 e2      -> failure exp
  BLeq e1 e2      -> failure exp
  BNeg _  b1      -> failure exp
  BAnd b1 _ b2    -> failure exp
  BOr  b1 _ b2    -> failure exp
  Ite bexp e1 e2  -> failure exp
  Abstr lam e     -> failure exp
  Rec _           -> failure exp
  Typed e ty      -> failure exp

-- | Helper function that combines the outcome of calling evalExp on two expressions
-- with a binary integer operation.
evalInt2 :: MonadReader Env m => (Integer -> Integer -> Integer) -> Exp -> Exp -> m Value
evalInt2 f e1 e2 = do
  r1 <- evalExp e1
  r2 <- evalExp e2
  case (r1, r2) of
    (VInt n1, VInt n2) -> pure $ VInt $ f n1 n2
    _ -> error $ "Expected integer values but got " ++ show r1 ++ " and " ++ show r2

-- | Helper function that combines the outcome of calling evalExp on one expression
-- with a unary Boolean operation.
evalBool1 :: MonadReader Env m => (Bool -> Bool) -> Exp -> m Value
evalBool1 f e = do
  r <- evalExp e
  case r of
    VBool b -> pure $ VBool $ f b
    _ -> error $ "Expected Boolean values but got " ++ show r

-- | Helper function that combines the outcome of calling evalExp on two expressions
-- with a binary Boolean operation.
evalBool2 :: MonadReader Env m => (Bool -> Bool -> Bool) -> Exp -> Exp -> m Value
evalBool2 f e1 e2 = do
  r1 <- evalExp e1
  r2 <- evalExp e2
  case (r1, r2) of
    (VBool b1, VBool b2) -> pure $ VBool $ f b1 b2
    _ -> error $ "Expected Boolean values but got " ++ show r1 ++ " and " ++ show r2
