{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartView.SmartView where

import Control.Applicative
import Control.Monad (MonadPlus (..), ap, liftM, (>=>))
import Data.Bifunctor
import Prelude hiding (Ordering (..), head, reverse, tail)

data List a
  = Nil
  | Cons a (List a)
  | List a :++ List a

wrap :: a -> List a
wrap = (`Cons` Nil)

data ListView a
  = NilV
  | ConsV a (List a)

-- Is there duplicate computation?
-- If ConsV x xs = viewL xs, the first evaluation will match NilP, then discards.
-- And evaluation will occurs again.

pattern NilP :: List a
pattern NilP <- (viewL -> NilV)

--   where
--     NilP = Nil

pattern ConsP :: a -> List a -> List a
pattern ConsP x xs <- (viewL -> ConsV x xs)

viewL :: List a -> ListView a
viewL = \case
  Nil -> NilV
  Cons x xs -> ConsV x xs
  (xs :++ ys) :++ zs -> viewL (xs :++ (ys :++ zs))
  -- Recursive pattern synonym definition with following bindings:
  --   NilP :++ right -> viewL right
  --   ConsP x xs :++ right -> ConsV x (xs :++ right)
  left :++ right -> case viewL left of
    NilV -> viewL right
    ConsV x xs -> ConsV x (xs :++ right)

-- Is there a double calculation? If `ConsV x xs = viewL xs`,
-- the first evaluation will match `NilP`, then discard, evaluate again.
reverse :: List a -> List a
-- Pattern match(es) are non-exhaustive
reverse (ConsP x xs) = reverse xs :++ wrap x
reverse NilP = Nil

-- reverse xs' = case viewL xs' of
--     NilV -> Nil
--     ConsV x xs -> reverse xs :++ wrap x

data Free f a
  = Var a
  | Con (f (Free f a))
  | forall x. (Free f x) :>>= (x -> Free f a)

data FreeView f a
  = VarV a
  | ConV (f (Free f a))

viewF :: (Functor f) => Free f a -> FreeView f a
viewF = \case
  Var x -> VarV x
  Con fx -> ConV fx
  ((mx :>>= f) :>>= g) -> viewF (mx :>>= (f >=> g))
  Var x :>>= f -> viewF (f x)
  Con t :>>= f -> ConV (fmap (:>>= f) t)

-- instance (Functor f) => Functor (Free f) where
--   fmap f (Var x) = Var (f x)
--   fmap f (Con fx) = Con (fmap (fmap f) fx)

-- instance (Functor f) => Applicative (Free f) where
--   pure = Var
--   Var f <*> fx = fmap f fx
--   Con ff <*> fx = Con (fmap (<*> fx) ff)

instance Functor (Free f) where
  fmap :: (a -> b) -> Free f a -> Free f b
  fmap = liftM

instance Applicative (Free f) where
  pure :: a -> Free f a
  pure = Var
  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  (<*>) = ap

instance Monad (Free f) where
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (>>=) = (:>>=)

-- instance (Functor f) => Monad (Free f) where
--   Var a >>= f = f a
--   Con t >>= f = Con (fmap (>>= f) t)
