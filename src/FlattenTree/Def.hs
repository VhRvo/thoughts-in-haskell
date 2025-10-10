module FlattenTree.Def where

data Tree a
  = Leaf a
  | Node (Tree a)
  | Cons (Tree a) (Tree a)

instance Semigroup (Tree a) where
  (<>) :: Tree a -> Tree a -> Tree a
  Cons ll lr <> r = ll <> (lr <> r)
--   Leaf a <> r = Cons (Leaf a) r
--   Node t <> r = Cons (Node t) r
  l <> r = Cons l r

_ = ()
  where
    append :: [a] -> [a] -> [a]
    append []       ys = ys
    append (x : xs) ys = x : append xs ys

    appendK :: [a] -> [a] -> ([a] -> [a]) -> [a]
    appendK []       ys k = k ys
    -- appendK (x : xs) ys k = k (x : append xs ys)
    appendK (x : xs) ys k = appendK xs ys (k . (x :))
