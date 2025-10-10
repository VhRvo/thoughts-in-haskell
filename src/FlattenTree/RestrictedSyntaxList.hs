module FlattenTree.RestrictedSyntaxList where

import FlattenTree.Def

data FItem a
  = FLeaf a
  | FNode (FTree a)

type FTree a
  = [FItem a]

flatten :: Tree a -> FTree a
flatten =
  \case
    Leaf a -> [FLeaf a]
    Node t -> [FNode (flatten t)]
    Cons (Leaf a) r -> FLeaf a : flatten r
    Cons (Node t) r -> FNode (flatten t) : flatten r
    Cons (Cons ll lr) r -> flatten (Cons ll (Cons lr r))

flatten1 :: Tree a -> FTree a
flatten1 =
  \case
    Leaf a -> [FLeaf a]
    Node t -> [FNode (flatten1 t)]
    Cons (Leaf a) r -> flatten1 (Leaf a) <> flatten1 r
    Cons (Node t) r -> flatten1 (Node t) <> flatten1 r
    Cons (Cons ll lr) r -> flatten1 (Cons ll (Cons lr r))

flatten2 :: Tree a -> FTree a
flatten2 =
  \case
    Leaf a -> [FLeaf a]
    Node t -> [FNode (flatten2 t)]
    Cons (Cons ll lr) r -> flatten2 (Cons ll (Cons lr r))
    Cons l r -> flatten2 l <> flatten2 r

flatten3 :: Tree a -> FTree a
flatten3 =
  \case
    Leaf a -> [FLeaf a]
    Node t -> [FNode (flatten3 t)]
    Cons (Cons ll lr) r -> flatten3 ll <> flatten3 lr <> flatten3 r
    Cons l r -> flatten3 l <> flatten3 r

flatten4 :: Tree a -> FTree a
flatten4 =
  \case
    Leaf a -> [FLeaf a]
    Node t -> [FNode (flatten4 t)]
    Cons (Cons ll lr) r -> flatten4 (Cons ll lr) <> flatten4 r
    Cons l r -> flatten4 l <> flatten4 r

flatten5 :: Tree a -> FTree a
flatten5 =
  \case
    Leaf a -> [FLeaf a]
    Node t -> [FNode (flatten5 t)]
    Cons l r -> flatten5 l <> flatten5 r

-- lemma flatten2 (Cons l r) = flatten2 l <> flatten2 r
-- case l of
--  Leaf a -> flatten2 (Cons (Leaf a) r) = flatten2 (Leaf a) <> flatten2 r
--  Node t -> flatten2 (Cons (Node t) r) = flatten2 (Node t) <> flatten2 r
--  Cons ll lr ->
--    flatten2 (Cons (Cons ll lr) r)
--  = flatten2 (Cons ll (Cons lr r))
--  = flatten2 ll <> flatten2 (Cons lr r)
--  = flatten2 ll <> flatten2 lr <> r
--  = flatten2 (Cons ll lr) <> r
--  qed.

-- to prove flatten2 = flatten5
-- lemma flatten2 (Cons ll (Cons lr r)) = flatten2 ll <> flatten2 lr <> flatten2 r
