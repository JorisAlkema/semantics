module Semantics.Delay where

import Control.Monad.Fix

-- | The Delay data type allows us to handle delayed computations, which are
-- computations that *may* happen in the future but do not have to. This type
-- allows us to implement partial functions from a to b as functions a -> Delay b.
-- Elements of the form Now a can be thought of as *finished* computations that have
-- a as result. Computations that finish after n steps with result a are of the form
-- Delay^n (Now a). Finally, non-terminationg computations are of the shape bot, see below.

data Delay a = Now a | Later (Delay a)
  deriving (Eq, Show)

coiter :: (b -> Either a b) -> b -> Delay a
coiter c x = case c x of
  Left a   -> Now a
  Right x' -> Later $ coiter c x'

boundedFold :: Int -> (b -> b) -> (a -> b) -> b -> Delay a -> b
boundedFold n f g b (Now a)   | n > 0 = g a
boundedFold n f g b (Later d) | n > 0 = f (boundedFold (n - 1) f g b d)
boundedFold n f g b d                 = b

-- | Prune an element of Delay to a finite portion, the depth of which is
-- indicated by the first parameter. A call pruneDelay n f b d results into
-- Later^k (f a) if d is of the form Later^k a for some k <= n, and
-- Later^n (Now b) otherwise.
pruneDelay :: Int -> (a -> b) -> b -> Delay a -> Delay b
pruneDelay n f b = boundedFold n Later (Now . f) (Now b)

-- | Traverses the argument until it finds Now x and returns x.
-- Only use this if you know that the argument will be finite, as the function otherwise
-- diverges.
unsafeFromNow :: Delay a -> a
unsafeFromNow (Now x)   = x
unsafeFromNow (Later d) = unsafeFromNow d

-- We can lift any map f : a -> b to a map fmap f :: Delay a -> Delay b
-- that aplies f only to finished computations.
instance Functor Delay where
  fmap f (Now a)   = Now (f a)
  fmap f (Later d) = Later (fmap f d)

instance Applicative Delay where
  pure = Now
  Now f    <*> d = fmap f d
  Later df <*> d = Later (df <*> d)

-- The Monad instance allows us to chain delayed computations together.
instance Monad Delay where
  Now a   >>= f = f a
  Later d >>= f = Later (d >>= f)

-- | Flat order
instance Eq a => Ord (Delay a) where
  Now x    <= Now y    = x == y
  Later d1 <= Now _    = True
  Now _    <= Later d2 = False
  Later d1 <= Later d2 = d1 <= d2

instance MonadFix Delay where
  mfix f =
    let x = f (unnow x)
    in x
    where
      unnow (Now a) = a

-- | The canonical non-terminating computation. This is is also the least element ⊥ in the
-- flat order defined above.
bot :: Delay a
bot = Later bot

-- | Kleisli composition
composeP :: (a -> Delay b) -> (b -> Delay c) -> a -> Delay c
composeP f g x = f x >>= g
