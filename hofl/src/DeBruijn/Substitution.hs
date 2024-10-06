{-# LANGUAGE RankNTypes #-}

module DeBruijn.Substitution (removeBinder, subst) where

import Control.Monad.Reader
import Control.Applicative

import DeBruijn.Abs

-- | subst x s e substitutes s for the free variable x in e.
subst :: String -> Exp -> Exp -> Exp
subst x s exp = case exp of
  Var   (Bound j _) -> exp
  Var   (Free y)    -> if x == y then s else exp
  Val   v           -> exp
  BVal  b           -> exp
  App   e1 e2       -> App (subst x s e1) (subst x s e2)
  Mul   e1 e2       -> Mul (subst x s e1) (subst x s e2)
  Add   e1 e2       -> Add (subst x s e1) (subst x s e2)
  Sub   e1 e2       -> Sub (subst x s e1) (subst x s e2)
  Pair  e1 e2       -> Pair (subst x s e1) (subst x s e2)
  BEq   e1 e2       -> BEq (subst x s e1) (subst x s e2)
  BLeq  e1 e2       -> BLeq (subst x s e1) (subst x s e2)
  BNeg  n  b        -> BNeg n (subst x s b)
  BAnd  b1 a b2     -> BAnd (subst x s b1) a (subst x s b2)
  BOr   b1 o b2     -> BOr (subst x s b1) o (subst x s b2)
  Ite   b e1 e2     -> Ite (subst x s b) (subst x s e1) (subst x s e2)
  Abstr lam e       -> Abstr lam (subst x s e)
  Rec   e           -> Rec (subst x s e)
  Typed e ty        -> Typed (subst x s e) ty

-- | removeBinder e s removes the outermost binder of e, if it exists,
-- and replaces the bound variable by s.
removeBinder :: Exp -> Exp -> Exp
removeBinder (Abstr _ e) s = runReader (substIdx s e) 0
removeBinder (Rec     e) s = runReader (substIdx s e) 0
removeBinder e           s = e


-- | Usafe internal operation: We assume that the ith de Bruijn index is
-- not bound and replace it with s in exp. This is not safe to use in general
-- because de Bruijn indices are supposed to refer to actual binders, which is
-- not the case here!
substIdx :: Exp -> Exp -> Reader Int Exp
substIdx s exp = case exp of
  Var   (Bound j _) -> asks (\i -> if i == j then s else exp)
  Var   (Free _)    -> return exp
  Val   v           -> return exp
  BVal  b           -> return exp
  App   e1 e2       -> substIdx2 App e1 e2
  Fst   e           -> Fst <$> substIdx s e
  Snd   e           -> Snd <$> substIdx s e
  Mul   e1 e2       -> substIdx2 Mul e1 e2
  Add   e1 e2       -> substIdx2 Add e1 e2
  Sub   e1 e2       -> substIdx2 Sub e1 e2
  Pair  e1 e2       -> substIdx2 Pair e1 e2
  BEq   e1 e2       -> substIdx2 BEq e1 e2
  BLeq  e1 e2       -> substIdx2 BLeq e1 e2
  BNeg  n  b        -> BNeg n <$> (substIdx s b)
  BAnd  b1 a b2     -> substIdx2 (\e -> BAnd e a) b1 b2
  BOr   b1 o b2     -> substIdx2 (\e -> BOr e o) b1 b2
  Ite   b e1 e2     -> liftA3 Ite (substIdx s b) (substIdx s e1) (substIdx s e2)
  Abstr lam e       -> Abstr lam <$> local (+1) (substIdx s e)
  Rec   e           -> Rec <$> local (+1) (substIdx s e)
  Typed e ty        -> liftA2 Typed (substIdx s e) (return ty)

  where
    substIdx2 :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Reader Int Exp
    substIdx2 f e1 e2 = liftA2 f (substIdx s e1) (substIdx s e2)
