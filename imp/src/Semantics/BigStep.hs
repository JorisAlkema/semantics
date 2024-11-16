{-# OPTIONS -Wall #-}

module Semantics.BigStep where

import Syntax.Abs
import Syntax.ErrM
import Semantics.Memory
import Semantics.ExpEval

evalCom :: Com -> Mem -> Mem
evalCom x mem = case x of
  CSkip -> mem
  CAssign ident aexp -> 
    let val = evalAExp aexp mem
    in updateMem ident val mem
  CSeq com1 com2 -> 
    let mem' = evalCom com1 mem
    in evalCom com2 mem'  
  CIte bexp com1 com2 -> 
    if evalBExp bexp mem 
    then evalCom com1 mem
    else evalCom com2 mem
  CWhile bexp com -> 
    if evalBExp bexp mem
      then 
      let mem' = evalCom com mem
      in evalCom (CWhile bexp com) mem'
    else mem --terminate

