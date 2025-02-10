module Infinite.DList where

import Data.Text (Text)

data DList a
  = Cons
  { elem :: a,
    prev :: DList a,
    next :: DList a
  }

-- instance (Show a) => Show (DList a) where
--     show :: DList a -> String
--     show (Cons elem _ _) = show elem

takeF :: Int -> DList a -> [a]
takeF 0 _ = []
takeF n (Cons x _ next) = x : takeF (n - 1) next

takeR :: Int -> DList a -> [a]
takeR 0 _ = []
takeR n (Cons x prev _) = x : takeR (n - 1) prev

mkCDList :: [a] -> DList a
mkCDList [] = error "double-linked list must have at least one element"
mkCDList as = first
  where
    (first, last) = go last as first
    go :: DList a -> [a] -> DList a -> (DList a, DList a)
    go prev [] next = (next, prev)
    go prev (a : as) next = (this, last)
      where
        this = Cons a prev rest
        (rest, last) = go this as next

test = takeF 10 $ mkCDList "ABCD"

-- x = head xs
-- map elem xs = as
-- map prev xs = rotR xs
-- map next xs = rotL xs
-- xs = zipWith3 Cons (map elem xs) (map prev xs) (map next xs)
rotR :: [a] -> [a]
rotR xs = last xs : init xs

rotL :: [a] -> [a]
rotL xs = tail xs <> [head xs]

mkCDList' :: [a] -> DList a
mkCDList' [] = error "double-linked list must have at least one element"
mkCDList' as = head xs
  where
    xs = zipWith3' Cons as (rotR xs) (rotL xs)

zipWith3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3' f (a : as) ~(b : bs) ~(c : cs) = f a b c : zipWith3' f as bs cs
zipWith3' f [] _ _ = []

test' = takeF 10 $ mkCDList' "ABCD"

mkCDList'' :: [a] -> Either Text (DList a)
mkCDList'' [] = Left "double-linked list must have at least one element"
mkCDList'' (a : as) = Right first
  where
    (first, last) = go last a first as
    go :: DList a -> a -> DList a -> [a] -> (DList a, DList a)
    go prev x nextOfLast xs = case xs of
      [] -> let current = Cons x prev nextOfLast in (current, current)
      y : ys ->
        let (rest, last) = go this y nextOfLast ys
            this = Cons x prev rest
         in (this, last)

test'' = takeR 10 <$> mkCDList'' "ABCD"

