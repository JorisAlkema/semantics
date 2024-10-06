{-# OPTIONS -Wall #-}

module Semantics.BigStep where

import Syntax.Abs
import Syntax.ErrM
import Semantics.Memory
import Semantics.ExpEval

evalCom :: Com -> Mem -> Mem
evalCom x = case x of
  CSkip -> failure x
  CAssign ident aexp -> failure x
  CSeq com1 com2 -> failure x
  CIte bexp com1 com2 -> failure x
  CWhile bexp com -> failure x
