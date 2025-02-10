module Encoding where

cPair :: a -> b -> forall r. (a -> b -> r) -> r
cPair x y k = k x y

sE1 :: (forall r. (a -> b -> r) -> r) -> a
sE1 p = p const

sE2 :: (forall r. (a -> b -> r) -> r) -> b
sE2 p = p (\_ y -> y)

swap :: (forall r. (a -> b -> r) -> r) -> forall r. (b -> a -> r) -> r
swap p = cPair (sE2 p) (sE1 p)

cPair' :: a -> b -> (a -> b -> c) -> c
cPair' x y k = k x y

sE1' :: ((a -> b -> a) -> a) -> a
sE1' p = p const

sE2' :: ((a -> b -> b) -> b) -> b
sE2' p = p (\_ y -> y)

swap' :: ((a -> a -> a) -> a) -> (a -> a -> r) -> r
swap' p = cPair' (sE2' p) (sE1' p)


