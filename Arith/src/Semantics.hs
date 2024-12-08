{-# OPTIONS -Wall #-}

module Semantics where

import Data.Set as Set
import Control.Monad
import Syntax.Abs

undef :: Show a => a -> String
undef x = "Undefined case: " ++ show x

failure :: Show a => a -> b
failure x = error $ "Undefined case: " ++ show x

unionWith :: (Ord c) => (a -> b -> c) -> Set a -> Set b -> Set c
unionWith f a = Set.map (uncurry f) . cartesianProduct a

-- | Lookup function for values in environment.
getVal :: Env -> Ident -> Double
getVal (Env e) = getVal_ e
  where
    getVal_ []               x = error $ "Variable " ++ show x ++ " not defined in environment."
    getVal_ (Assign y v : l) x =
      if x == y then v else getVal_ l x

-- | Domain for denotational semantics. Adds an error element to the rational numbers
-- Since Maybe is a monad, we can use fmap, liftM2 etc.
type D = Maybe Double

-- | Implements the denotational semantics
denotation :: Env -> Exp -> D
denotation env e = case e of
  ENum   k     -> Just k
  EVar   ident -> Just $ getVal env ident
  ETimes e1 e2 -> liftM2 (*) (denotation env e1) (denotation env e2) 
  EDiv   e1 e2 -> do 
    x <- denotation env e1
    y <- denotation env e2
    if y /= 0 then Just (x/y) else Nothing
  EPlus  e1 e2 -> liftM2 (+) (denotation env e1) (denotation env e2) 
-- ^ do notations allows us to check if y /= 0 before and then using Just or Nothing. 

-- | Implements the big-step operational semantics. Note that bigStep can be a function because
-- the relation is deterministic.

bigStep :: Env -> Exp -> Set Double
bigStep env e = case e of
  ENum   k     -> singleton k
  EVar   ident -> singleton $ getVal env ident
  ETimes e1 e2 -> Set.fromList [v1 * v2 | v1 <- Set.toList (bigStep env e1), v2 <- Set.toList (bigStep env e2)]
  EDiv   e1 e2 -> Set.fromList [v1 / v2 | v1 <- Set.toList (bigStep env e1), v2 <- Set.toList (bigStep env e2), v2 /= 0]
  EPlus  e1 e2 -> Set.fromList [v1 + v2 | v1 <- Set.toList (bigStep env e1), v2 <- Set.toList (bigStep env e2)]
-- ^ I think I could have also used unionWith here but did not understand id and I ended up using just set to list and list comphrehansions


-- | Implements the small-step semantics. Note that this only gives one computation step.
-- The return type is a _set_ because the small-step semantics don't prescribe an evaluation
-- strategy, this is different for smallStepLeftFirst.
-- To fully compute a value, we have to use the reflexive-transitive closure, see
-- normaliseSmallSteps below.
smallStep :: (Env, Exp) -> Set (Env, Exp)
smallStep (env, e) = case e of
  ENum   _     -> empty
  EVar   ident -> singleton (env, ENum $ getVal env ident)
  ETimes e1 e2 -> binaryS e1 e2 (\x -> Just . (*) x) ETimes
  EDiv   e1 e2 -> binaryS e1 e2 (\x y -> if y /= 0 then Just (x / y) else Nothing) EDiv
  EPlus  e1 e2 -> binaryS e1 e2 (\x -> Just . (+) x) EPlus
  -- ^ Using BinaryS so the maybe double type can be used. using Just and Nothing for the div. and always just for the plus and times.

  where
    binaryS :: Exp -> Exp ->
               (Double -> Double -> Maybe Double) ->
               (Exp -> Exp -> Exp) ->
               Set (Env, Exp)
    binaryS e1 e2 op cons =
      let s1 = Set.map (\(env1, eL) -> (env1, cons eL e2)) $ smallStep (env, e1)
          s2 = Set.map (\(env1, eR) -> (env1, cons e1 eR)) $ smallStep (env, e2)
          s3 = case (e1, e2) of
            (ENum n1, ENum n2) ->
              case op n1 n2 of
                Just n -> singleton (env, ENum n)
                Nothing -> empty
            _ -> empty
      in s1 `union` s2 `union` s3

--
--  --------------------
--  k1 (x) k2 -> k1 * k2

-- should be read in the abstract syntax here as

--
--  ---------------------------------------- (prod)
--  ETimes (EInt k1) (EInt k2) -> EInt (k1 * k2)

-- and similarly for the other rules

--     e1 -> e1'
--  --------------------------
--  e1 (x) e2 -> e1' (x) e2

--     e2 -> e2'
--  --------------------
--  e1 (x) e2 -> e1 (x) e2'



-- | Uses the small-semantics to compute a normal form of the given expression.
-- This is possible because the small-step semantics for this language is normalising.
-- Moreover, we can return just one expression because the small-step semantics are
-- deterministic, which allows us to pick any expression to progress with.
normaliseSmallSteps :: (Env, Exp) -> (Env, Exp)
normaliseSmallSteps (env, e) = case e of
  ENum _ -> (env, e)
  _      -> normaliseSmallSteps $ findMin $ smallStep (env, e)

-- | Implements one step of the small-step semantics with a fixed evaluation strategy to
-- resolve the non-determinism. As strategy, we choose to evaluate terms on the left of
-- an operator first to a value, and only allow computation steps on the left. This strategy
-- is described in the MoC book on page 12. Note that smallStepLeftFirst does not have to return
-- a set because it implements a deterministic strategy.
smallStepLeftFirst :: (Env, Exp) -> (Env, Exp)
smallStepLeftFirst (env, e) = case e of
  ENum   _     -> (env, e)
  EVar   x     -> (env, ENum $ getVal env x)
  ETimes e1 e2 -> binaryL e1 e2 (*) ETimes
  EPlus  e1 e2 -> binaryL e1 e2 (+) EPlus
  EDiv   e1 e2 -> binaryL e1 e2 (/) EDiv

  where
    binaryL (ENum n1) (ENum n2) op _   =
      (env, ENum $ op n1 n2)
    binaryL (ENum n1) e2        _ cons =
      (env, cons (ENum n1) (snd $ smallStepLeftFirst (env, e2)))
    binaryL e1        e2        _ cons =
      (env, cons (snd $ smallStepLeftFirst (env, e1)) e2)

-- | Computes a normal form of the given expression by using the left-first evaluation
-- strategy.
normaliseSmallStepsLeft :: (Env, Exp) -> (Env, Exp)
normaliseSmallStepsLeft (env, e) = case e of
  ENum _ -> (env, e)
  _      -> normaliseSmallStepsLeft $ smallStepLeftFirst (env, e)
