{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeFamilies where

import Data.Functor.Compose

type family FMap (m :: a -> b) (x :: f a) :: f b

-- FMap :: (a -> b) -> ([a] -> [b])
type instance FMap m '[] = '[]

type instance FMap m (v ': vs) = m v ': FMap m vs

-- FMap :: (a -> b) -> ((r -> a) -> (r -> b))
type instance FMap m n = Compose m n

-- type instance FMap m (n :: (r -> a)) = Compose m n
-- Whereas at the term level, functions can be defined by pattern-matching on their arguments, at the type level, type families can be defined by pattern-matching not only on their arguments but also their kinds. Notice that in the last type family instance above, we actually don’t inspect m and n, but the type family will check whether the kind of n is an arrow r -> a in order to use that instance.
