{-# LANGUAGE BangPatterns #-}

module Circular.List where

trace :: (a -> c -> (b, c)) -> a -> b
trace f a = b
  where
    (b, !c) = f a c

-- >> repMinList' [1..10] 0
repMinList' :: (Ord a) => [a] -> a -> ([a], a)
repMinList' [x] m = ([m], x)
repMinList' (x:xs) m =
    let (replaced, m') = repMinList' xs m
     in (m : replaced, min x m')

repMinList :: (Ord a) => [a] -> [a]
repMinList = trace repMinList'

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


