-- {-# LANGUAGE No #-}

module CPS where

func :: (forall a. a -> a) -> a -> b -> (a, b)
func f a b = (f a, f b)
