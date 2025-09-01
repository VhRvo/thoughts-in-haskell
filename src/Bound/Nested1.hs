{-# LANGUAGE DeriveTraversable #-}

module Bound.Nested1 where

import Data.Foldable
import Data.Foldable qualified as Foldable
import Data.Traversable
import Control.Monad (ap)

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam (Expr (Maybe a))
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
  Lam body >>= f = Lam (body >>= maybe (Var Nothing) (fmap Just . f))
