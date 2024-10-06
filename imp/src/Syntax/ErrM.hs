{-# LANGUAGE ImplicitParams #-}
-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta
-- Bumped to new monad hierarchy for this project by Henning Basold

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module Syntax.ErrM where

-- the Error monad: like Maybe type with error msgs

import GHC.Stack
import Control.Monad (MonadPlus(..), liftM)
import Control.Applicative (Alternative(..))

data Err a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad Err where
  Ok a  >>= f = f a
  Bad s >>= _ = Bad s

instance MonadFail Err where
  fail = Bad

instance Applicative Err where
  pure = Ok
  (Bad s) <*> _ = Bad s
  (Ok f) <*> o  = liftM f o

instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad "Err.mzero"
  mplus (Bad _) y = y
  mplus x       _ = x

instance Alternative Err where
  empty = mzero
  (<|>) = mplus

-- failure :: (Show a, Monad m, ?loc :: CallStack) => a -> m (Err b)
-- failure x = return $ fail $ "Undefined case: " ++ show x ++ "\n" ++ show ?loc

failure :: (Show a, ?loc :: CallStack) => a -> b
failure x = error $ "Undefined case: " ++ show x ++ "\n" ++ show ?loc
