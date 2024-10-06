{-# LANGUAGE FlexibleContexts #-}

module DeBruijn.Translate (toDeBruijnTree) where

import Control.Monad.Reader
import Control.Applicative
import Data.List

import Syntax.Abs as Raw
import qualified DeBruijn.Abs as DB
import ErrM

-- Turns a raw syntax tree into a de Bruijn tree with numbered identifiers.
toDeBruijnTree :: Raw.Exp -> DB.Exp
toDeBruijnTree e = runReader (toDeBruijn e) []

translateIdent :: MonadReader [Raw.Ident] m => Raw.Ident -> m DB.Ident
translateIdent i = asks (f i)
  where
    f i l = case (elemIndex i l, i) of
      (Nothing, Ident x) -> DB.Free x
      (Just n,  Ident x) -> DB.Bound n x

trans2 :: MonadReader [Raw.Ident] m =>
  (DB.Exp -> DB.Exp -> DB.Exp) -> Raw.Exp -> Raw.Exp -> m DB.Exp
trans2 cons e1 e2 = liftA2 cons (toDeBruijn e1) (toDeBruijn e2)

-- The list in the reader is the list of bound identifiers. The order of identifers in the
-- list corresponds to the binding depth: the innermost bound identifier is at the head of
-- the list.
toDeBruijn :: MonadReader [Raw.Ident] m => Raw.Exp -> m DB.Exp
toDeBruijn x = case x of
  Var   i -> DB.Var <$> translateIdent i
  Val   v -> return $ DB.Val v
  BVal  c -> return $ DB.BVal c
  App   e1 e2 -> trans2 DB.App e1 e2
  Fst   e     -> DB.Fst <$> toDeBruijn e
  Snd   e     -> DB.Snd <$> toDeBruijn e
  Mul   e1 e2 -> trans2 DB.Mul e1 e2
  Add   e1 e2 -> trans2 DB.Add e1 e2
  Sub   e1 e2 -> trans2 DB.Sub e1 e2
  Pair  e1 e2 -> trans2 DB.Pair e1 e2
  BEq   e1 e2 -> trans2 DB.BEq e1 e2
  BLeq  e1 e2 -> trans2 DB.BLeq e1 e2
  BNeg  neg e -> DB.BNeg neg <$> toDeBruijn e
  BAnd  e1 and e2 -> trans2 (\e -> DB.BAnd e and) e1 e2
  BOr   e1 or  e2 -> trans2 (\e -> DB.BOr e or) e1 e2
  Ite   bexp e1 e2 -> liftA3 DB.Ite (toDeBruijn bexp) (toDeBruijn e1) (toDeBruijn e2)
  Abstr lam i e -> DB.Abstr lam <$> local (\l -> i : l)  (toDeBruijn e)
  Rec   i e -> DB.Rec <$> local (\l -> i : l)  (toDeBruijn e)
  Typed e ty -> liftA2 DB.Typed (toDeBruijn e) (pure ty)
