{-# LANGUAGE DeriveTraversable #-}

module Bound.Scope1 where

import Control.Monad (ap, guard)
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable
import Data.Foldable qualified as Foldable
import Data.Traversable

newtype Scope f a = Scope {runScope :: f (Maybe a)}
  deriving (Functor, Foldable, Traversable)

instance (Monad f) => Applicative (Scope f) where
  pure :: forall f a. (Monad f) => a -> Scope f a
  pure = Scope . pure . Just
  (<*>) :: forall f a b. (Monad f) => Scope f (a -> b) -> Scope f a -> Scope f b
  (<*>) = ap

instance (Monad f) => Monad (Scope f) where
  (>>=) :: forall f a b. (Monad f) => Scope f a -> (a -> Scope f b) -> Scope f b
  Scope m >>= f = Scope (m >>= maybe (pure Nothing) (runScope . f))

--   Scope m >>= f = Scope $
--     m >>= \case
--       Nothing -> pure Nothing
--       Just x -> runScope (f x)

instance MonadTrans Scope where
  lift :: forall m a. (Monad m) => m a -> Scope m a
  -- requires touching every leaf in the expression
  lift = Scope . fmap Just

abstract :: forall f a. (Functor f, Eq a) => a -> f a -> Scope f a
abstract x fx = Scope (fmap go fx)
  where
    go y = y <$ guard (x /= y)

instantiate :: forall f a. (Monad f) => f a -> Scope f a -> f a
instantiate x (Scope fx) = fx >>= go
  where
    go = maybe x pure

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam (Scope Expr a)
  deriving (Functor, Foldable, Traversable)

instance Applicative Expr where
  pure :: forall a. a -> Expr a
  pure = Var
  (<*>) :: forall a b. Expr (a -> b) -> Expr a -> Expr b
  (<*>) = ap

instance Monad Expr where
  (>>=) :: forall a b. Expr a -> (a -> Expr b) -> Expr b
  Var a >>= f = f a
  App fun arg >>= f =
    App (fun >>= f) (arg >>= f)
  -- Lam body >>= f = body >>= maybe (Lam (Var Nothing)) f
  Lam body >>= f = Lam (body >>= (lift . f))
