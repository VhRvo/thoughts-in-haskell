{-# LANGUAGE FlexibleInstances #-}

module CategoryFunctor where

import Data.Kind
import Prelude hiding (id, (.))

-- type Category :: (o :: Type) -> (o -> o -> Type) -> Constraint
-- class Category o m where
class Category (o :: Type) (m :: o -> o -> Type) where
  id :: m a a
  (.) :: m b c -> m a b -> m a c

-- data Nat = Zero | Succ

instance Category Type (->) where
  id x = x
  (g . f) x = g (f x)

class
  (Category o1 m1, Category o2 m2) =>
  Functor o1 (m1 :: o1 -> o1 -> Type) o2 (m2 :: o2 -> o2 -> Type) (objMap :: o1 -> o2)
  where
  arrowMap :: m1 a b -> m2 (objMap a) (objMap b)
