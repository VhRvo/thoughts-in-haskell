{-# LANGUAGE DeriveTraversable #-}

module Bound.Scope2 where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe (fromMaybe)

newtype Scope f a
  = Scope {runScope :: f (Maybe (f a))}
  deriving (Functor, Foldable, Traversable)

instance (Monad f) => Applicative (Scope f) where
  pure :: forall f a. (Monad f) => a -> Scope f a
  pure = Scope . pure . Just . pure

  (<*>) :: forall f a b. (Monad f) => Scope f (a -> b) -> Scope f a -> Scope f b
  (<*>) = ap

instance (Monad f) => Monad (Scope f) where
  (>>=) :: forall f a b. (Monad f) => Scope f a -> (a -> Scope f b) -> Scope f b
  Scope body >>= f = Scope (body >>= go)
    where
      go :: Maybe (f a) -> f (Maybe (f b))
      go Nothing = pure Nothing
      go (Just x) = x >>= (runScope . f)

instance MonadTrans Scope where
  lift :: forall m a. (Monad m) => m a -> Scope m a
  -- no longer requires touching every leaf in the expression
  lift = Scope . pure . Just

-- still requires touching every leaf in the expression
-- lift = Scope . fmap (pure . pure)

abstract :: forall f a. (Monad f, Eq a) => a -> f a -> Scope f a
abstract x fx = Scope (fmap go fx)
  where
    go :: a -> Maybe (f a)
    go y = pure y <$ guard (x /= y)

instanitiate :: forall f a. (Monad f) => f a -> Scope f a -> f a
instanitiate fx (Scope body) = body >>= go
  where
    go :: Maybe (f a) -> f a
    go = fromMaybe fx

instanitiate' :: forall f a. (Monad f) => a -> Scope f a -> f a
instanitiate' x (Scope body) = body >>= go
  where
    go :: Maybe (f a) -> f a
    go Nothing = pure x
    go (Just fx) = fx
