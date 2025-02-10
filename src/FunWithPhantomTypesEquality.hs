module FunWithPhantomTypesEquality where

newtype a :=: b = Proof { apply :: forall f. f a -> f b }

refl :: forall a. a :=: a
refl = Proof id

newtype Flip f a b = Flip { unFlip :: f b a }
