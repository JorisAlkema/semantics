module Semantics.Memory where

import Data.HashMap.Strict
import Syntax.Abs

-- | The type of memory that holds for locations a corresponding value.
type Mem = HashMap Ident Integer

-- | Memory without any predefined values.
emptyMem :: Mem
emptyMem = empty

-- | Initialises a memory with values given by an environment.
mkMem :: Env -> Mem
mkMem (Env e) = fromList $ fmap (\(Assign x n) -> (x, n)) e

-- | Updates some memory at a location given by an Ident to contain the Integer after the update.
-- This corresponds to σ[x |-> n] for memory σ, location x and integer n.
updateMem :: Ident -> Integer -> Mem -> Mem
updateMem = insert

-- | Retrieves a value from a memory location with 0 as initial value.
getVal :: Ident -> Mem -> Integer
getVal = lookupDefault 0
