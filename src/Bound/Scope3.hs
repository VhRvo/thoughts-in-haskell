{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DefaultSignatures #-}

module Bound.Scope3 where

import Control.Monad
import Control.Monad.Trans

data Var b a
  = Bound b
  | Free a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative (Var b) where
  pure :: forall b a. a -> Var b a
  pure = Free
  (<*>) :: forall b a c. Var b (a -> c) -> Var b a -> Var b c
  (<*>) = ap

instance Monad (Var b) where
  (>>=) :: forall b a c. Var b a -> (a -> Var b c) -> Var b c
  Bound bound >>= _ = Bound bound
  Free x      >>= f = f x

newtype Scope b f a = Scope { runScope :: f (Var b (f a)) }
  -- deriving (Eq, Ord, Show)
  deriving (Functor, Foldable, Traversable)

instance (Monad f) => Applicative (Scope b f) where
  pure :: forall f b a. (Monad f) => a -> Scope b f a
  pure = Scope . pure . Free . pure

  (<*>) :: forall f b a c. (Monad f) => Scope b f (a -> c) -> Scope b f a -> Scope b f c
  (<*>) = ap

instance (Monad f) => Monad (Scope b f) where
  (>>=) :: forall f b a c. (Monad f) => Scope b f a -> (a -> Scope b f c) -> Scope b f c
  Scope body >>= f = Scope (body >>= go)
    where
      go :: Var b (f a) -> f (Var b (f c))
      go (Bound name) = pure (Bound name)
      go (Free name)  = name >>= runScope . f

instance MonadTrans (Scope b) where
  lift :: forall f b a. (Monad f) => f a -> Scope b f a
  lift = Scope . pure. Free

class Bound t where
  (>>>=) :: Monad f => t f a -> (a -> f b) -> t f b
  default (>>>=) :: (MonadTrans t, Monad f) => t f a -> (a -> f b) -> t f b
  mx >>>= f = mx >>= lift . f

instance Bound (Scope b)

abstract :: forall f b a. (Monad f) => (a -> Maybe b) -> f a -> Scope b f a
abstract f fx = Scope (fmap go fx)
  where
    go :: a -> Var b (f a)
    go x = case f x of
      Nothing -> Free (pure x)
      Just bound -> Bound bound

instantiate :: forall f b a. (Monad f) => (b -> f a) -> Scope b f a -> f a
instantiate k (Scope body) = body >>= go
  where
    go :: Var b (f a) -> f a
    go (Bound bound) = k bound
    go (Free fx)     = fx


