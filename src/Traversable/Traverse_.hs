{-# LANGUAGE ScopedTypeVariables #-}

module Traversable.Traverse_ where

import Data.Coerce
import Data.Functor (void)
import Data.Functor.Const
import Data.Monoid

traverse_ :: forall t f a b. (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
-- traverse_ transform = getAp . getConst . foldMap (\x -> Const (Ap (void $ transform x)))
-- traverse_ transform = coerce (foldMap @t @(Const (Ap f ()) ()) (\x -> coerce (void $ transform x)))
traverse_ transform =
  coerce
    ( foldMap @t @(Const (Ap f ()) ())
        (coerce @_ @(a -> Const (Ap f ()) ()) (void . transform))
    )


