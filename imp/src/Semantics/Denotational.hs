{-# OPTIONS -Wall #-}

module Semantics.Denotational where

import Syntax.Abs
import Syntax.ErrM
import Semantics.Memory
import Semantics.ExpEval
import Semantics.Delay

-- | Domain type that we will be computing in. This is essentially a type
-- of partial functions from memory to a.
type D a = Mem -> Delay a

-- | Implements the denotational semantics of IMP.
-- This maps should be implemented so that every step in a while loop
-- generates one Later step, see the test cases.
evalCom :: Com -> D Mem
evalCom c = case c of
  CSkip            -> pure -- η from lecture
  CAssign x  e     -> \mem -> Now (updateMem x (evalAExp e mem) mem)
  CSeq    c1 c2    -> \mem -> evalCom c1 mem >>= evalCom c2
  CIte    b  c1 c2 -> \mem -> if evalBExp b mem 
                              then evalCom c1 mem 
                              else evalCom c2 mem
  CWhile b c1      -> \mem -> if evalBExp b mem
                              then Later (evalCom c1 mem >>= evalCom (CWhile b c1))
                              else Now mem
