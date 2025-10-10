module FlattenTree.RestrictedSyntaxNonEmpty where

import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import FlattenTree.Def
import Data.Foldable (Foldable(toList))

data FItem a
  = FLeaf a
  | FNode (FTree a)

type FTree a
  = NonEmpty (FItem a)

flatten :: Tree a -> FTree a
flatten =
  \case
    Leaf a -> NE.singleton (FLeaf a)
    Node t -> NE.singleton (FNode (flatten t))
    Cons (Leaf a) r -> FLeaf a <| flatten r
    Cons (Node t) r -> FNode (flatten t) <| flatten r
    Cons (Cons ll lr) r -> flatten (Cons ll (Cons lr r))

flatten1 :: Tree a -> FTree a
flatten1 =
  \case
    Leaf a -> NE.singleton (FLeaf a)
    Node t -> NE.singleton (FNode (flatten1 t))
    Cons (Leaf a) r -> flatten1 (Leaf a) <> flatten1 r
    Cons (Node t) r -> flatten1 (Node t) <> flatten1 r
    Cons (Cons ll lr) r -> flatten1 (Cons ll (Cons lr r))

flatten2 :: Tree a -> FTree a
flatten2 =
  \case
    Leaf a -> NE.singleton (FLeaf a)
    Node t -> NE.singleton (FNode (flatten2 t))
    Cons (Cons ll lr) r -> flatten2 (Cons ll (Cons lr r))
    Cons l r -> flatten2 l <> flatten2 r

flatten3 :: Tree a -> FTree a
flatten3 =
  \case
    Leaf a -> NE.singleton (FLeaf a)
    Node t -> NE.singleton (FNode (flatten3 t))
    Cons (Cons ll lr) r -> flatten3 ll <> flatten3 lr <> flatten3 r
    Cons l r -> flatten3 l <> flatten3 r

flatten4 :: Tree a -> FTree a
flatten4 =
  \case
    Leaf a -> NE.singleton (FLeaf a)
    Node t -> NE.singleton (FNode (flatten4 t))
    Cons (Cons ll lr) r -> flatten4 (Cons ll lr) <> flatten4 r
    Cons l r -> flatten4 l <> flatten4 r

flatten5 :: Tree a -> FTree a
flatten5 =
  \case
    Leaf a -> NE.singleton (FLeaf a)
    Node t -> NE.singleton (FNode (flatten5 t))
    Cons l r -> flatten5 l <> flatten5 r

flatten6 :: Tree a -> [FItem a] -> FTree a
flatten6 tree acc =
  case tree of
    Leaf a -> FLeaf a :| acc
    Node t -> FNode (flatten6 t []) :| acc
    Cons l r -> flatten6 l (toList (flatten6 r acc))
