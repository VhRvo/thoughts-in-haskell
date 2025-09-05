{-# LANGUAGE DeriveTraversable #-}

module Bound.Scope3Demo where

import Bound.Scope3
import Control.Monad
import Control.Monad.Trans (lift)

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam (Scope () Expr a)
  | Let [Scope Int Expr a] (Scope Int Expr a)
  deriving (Functor, Foldable, Traversable)

instance Applicative Expr where
  pure :: forall a. a -> Expr a
  pure = Var
  (<*>) :: forall a b. Expr (a -> b) -> Expr a -> Expr b
  (<*>) = ap

instance Monad Expr where
  (>>=) :: forall a b. Expr a -> (a -> Expr b) -> Expr b
  Var var >>= f =
    f var
  App fun arg >>= f =
    App (fun >>= f) (arg >>= f)
  Lam body >>= f =
    -- Lam (body >>= lift . f)
    Lam (body >>>= f)
  Let bindings body >>= f =
    Let
      -- (fmap (>>= lift . f) bindings)
      -- (body >>= lift . f)
      (fmap (>>>= f) bindings)
      (body >>>= f)
