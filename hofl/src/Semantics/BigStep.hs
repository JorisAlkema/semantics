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
        
    Fst e -> do
      v <- evalExp e
      case v of
        VPair v1 _ -> pure v1
        _ -> error $ "Expected pair in fst, but got: " ++ show v

    Snd e -> do
      v <- evalExp e
      case v of
        VPair _ v2 -> pure v2
        _ -> error $ "Expected pair in snd, but got: " ++ show v
    
    Mul e1 e2 -> evalInt2 (*) e1 e2
    Add e1 e2 -> evalInt2 (+) e1 e2
    Sub e1 e2 -> evalInt2 (-) e1 e2
    
    Pair e1 e2 -> do
      v1 <- evalExp e1
      v2 <- evalExp e2
      pure $ VPair v1 v2
    
    BEq e1 e2 -> do
      v1 <- evalExp e1
      v2 <- evalExp e2
      case (v1, v2) of
        (VInt n1, VInt n2)       -> pure $ VBool (n1 == n2)
        (VBool b1, VBool b2)     -> pure $ VBool (b1 == b2)
        (VPair a1 b1, VPair a2 b2) -> do
          -- Deep comparison 
          vA <- evalExp (BEq (toExp a1) (toExp a2))
          vB <- evalExp (BEq (toExp b1) (toExp b2))
          case (vA, vB) of
            (VBool resA, VBool resB) -> pure $ VBool (resA && resB)
            _ -> error "Type mismatch in pair comparison"
        _ -> error $ "cannot compare " ++ show v1 ++ " and " ++ show v2"

    BLeq e1 e2 -> do
      v1 <- evalExp e1
      v2 <- evalExp e2
      case (v1, v2) of
        (VInt n1, VInt n2) -> pure $ VBool (n1 <= n2)
        _ -> error $ "Expected integer values in BLeq, but got: " ++ show v1 ++ " and " ++ show v2

    BNeg _ b1 -> evalBool1 not b1
    BAnd b1 _ b2 -> evalBool2 (&&) b1 b2
    BOr b1 _ b2 -> evalBool2 (||) b1 b2
    Ite bexp e1 e2 -> do
      cond <- evalExp bexp
      case cond of
        VBool True  -> evalExp e1
        VBool False -> evalExp e2
        _ -> error $ "expected Boolean in Ite condition, got: " ++ show cond

    Abstr lam e -> pure $ VThunk (Abstr lam e)

    Rec x e -> do
      --create a thunk ther refers to itself
      --in a new env where x is bound to that thunk
      --we capture current env with local
      env <- ask
      let recursiveThunk = VThunk e
      let extendedEnv = HM.insert x (toDeBruijnTree recursiveThunk) env
      local (const extendedEnv) (evalExp e)

    Typed e ty -> evalExp e

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
