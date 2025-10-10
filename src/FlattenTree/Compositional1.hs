module FlattenTree.Compositional1 where

import FlattenTree.Def

flatten :: Tree a -> Tree a
flatten =
  \case
    Leaf a -> Leaf a
    Node t -> Node (flatten t)
    Cons (Leaf a) r -> Cons (Leaf a) (flatten r)
    Cons (Node t) r -> Cons (Node (flatten t)) (flatten r)
    Cons (Cons ll lr) r -> flatten (Cons ll (Cons lr r))

flatten1 :: Tree a -> Tree a
flatten1 =
  \case
    Leaf a -> Leaf a
    Node t -> Node (flatten1 t)
    Cons (Leaf a) r -> Cons (flatten1 (Leaf a)) (flatten1 r)
    Cons (Node t) r -> Cons (flatten1 (Node t)) (flatten1 r)
    Cons (Cons ll lr) r -> flatten1 (Cons ll (Cons lr r))

flatten2 :: Tree a -> Tree a
flatten2 =
  \case
    Leaf a -> Leaf a
    Node t -> Node (flatten2 t)
    Cons (Cons ll lr) r -> flatten2 (Cons ll (Cons lr r))
    Cons l r -> Cons (flatten2 l) (flatten2 r)
