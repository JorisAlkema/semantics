-- | Implements the evaluation of arithmetic and Boolean expressions.
-- As this is basically the same for big-step and denotational semantics,
-- we abstract this into one module.
-- Technically, big-step semantics would be given by a relation, but as
-- this relation is functional for expressions in IMP, we can just as well
-- write it immediately as function.
module Semantics.ExpEval where

import Control.Applicative
import Syntax.Abs
import Syntax.ErrM
import Semantics.Memory

evalAExp :: AExp -> Mem -> Integer
evalAExp a = case a of
  EInt   n     -> const n
  EVar   x     -> getVal x
  ETimes a1 a2 -> liftA2 (*) (evalAExp a1) (evalAExp a2)
  EPlus  a1 a2 -> liftA2 (+) (evalAExp a1) (evalAExp a2)
  ESub   a1 a2 -> liftA2 (-) (evalAExp a1) (evalAExp a2)

evalBVal :: BVal -> Mem -> Bool
evalBVal v m = case v of
  BTrue  -> True
  BFalse -> False

evalBExp :: BExp -> Mem -> Bool
evalBExp b = case b of
  EVal v       -> failure b
  EEq  a1 a2   -> liftA2 (==) (evalAExp a1) (evalAExp a2)
  ELeq a1 a2   -> liftA2 (<=) (evalAExp a1) (evalAExp a2)
  ENeg _  b1   -> failure b
  EAnd b1 _ b2 -> failure b
  AOr  b1 _ b2 -> failure b
