{-# LANGUAGE DeriveTraversable #-}

module Bound.DeBruijn where

import Data.Foldable
import qualified Data.Foldable as Foldable
import Data.Traversable

newtype Scope f a = Scope (f a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Expr a
  = Free a
  | Bound !Int
  | App (Expr a) (Expr a)
  | Lam (Scope Expr a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

abstract :: forall a. (Eq a) => a -> Expr a -> Scope Expr a
abstract me expr = Scope (letMeBound 0 expr)
  where
    letMeBound this (Free you)
      | me == you = Bound this
      | otherwise = Free you
    letMeBound this (Bound that) = Bound that
    letMeBound this (App fun arg) = letMeBound this fun `App` letMeBound this arg
    letMeBound this (Lam (Scope body)) = letMeBound (succ this) body

instantiate :: forall a. Expr a -> Scope Expr a -> Expr a
instantiate what (Scope body) = what'sBound 0 body
  where
    what'sBound this (Bound that)
      | this == that = what
      | otherwise    = Bound that
    what'sBound _    (Free you) = Free you
    what'sBound this (App fun arg) = what'sBound this fun `App` what'sBound this arg
    what'sBound this (Lam (Scope body)) = Lam (Scope (what'sBound (succ this) body))

closed :: forall f a b. (Traversable f) => f a -> Maybe (f b)
closed = traverse (const Nothing)

isClosed :: forall f a. (Foldable f) => f a -> Bool
isClosed = Foldable.all (const False)
