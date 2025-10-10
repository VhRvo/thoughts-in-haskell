module FlattenTree.Essential where

import FlattenTree.Def

-- lemma: flatten (Cons l r) = flatten l <> flatten r
-- case l of
--  Leaf a -> flatten (Cons (Leaf a) r) = flatten (Leaf a) <> flatten r
--  Node t -> flatten (Cons (Node t) r) = flatten (Node t) <> flatten r
--  Cons ll lr ->
--              flatten (Cons (Cons ll lr) r)
--  ={ def   }= flatten (Cons ll (Cons lr r))
--  ={ ind   }= flatten ll <> flatten (Cons lr r)
--  ={ ind   }= flatten ll <> (flatten lr <> flatten r)
--  ={ assoc }= (flatten ll <> flatten lr) <> flatten r
--  ={ ind   }= flatten (Cons ll lr) <> flatten r
--  qed.
flatten :: Tree a -> Tree a
flatten =
  \case
    Leaf a -> Leaf a
    Node t -> Node (flatten t)
    Cons (Cons ll lr) r -> flatten (Cons ll (Cons lr r))
    Cons l r -> flatten l <> flatten r

flatten1 :: Tree a -> Tree a
flatten1 =
  \case
    Leaf a -> Leaf a
    Node t -> Node (flatten1 t)
    -- Cons (Cons ll lr) r -> flatten1 ll <> flatten1 lr  flatten1 r
    Cons l r -> flatten1 l <> flatten1 r

-- apply : NonConsTree a -> MaybeEmptyTree a -> FlattenedTree a
apply :: Tree a -> Maybe (Tree a) -> Tree a
apply nonConsTree Nothing    = nonConsTree
apply nonConsTree (Just acc) = Cons nonConsTree acc

flatten2 :: Tree a -> Maybe (Tree a) -> Tree a
flatten2 tree acc =
  case tree of
    Leaf a -> apply (Leaf a) acc
    Node t -> apply (Node (flatten2 t Nothing)) acc
    Cons l r -> flatten2 l (Just (flatten2 r acc))

flatten3 :: Tree a -> (Tree a -> Tree a) -> Tree a
flatten3 tree k =
  case tree of
    Leaf a -> k (Leaf a)
    Node t -> k (Node (flatten3 t id))
    Cons l r -> flatten3 l $ \l' -> flatten3 r $ \r' -> k (l' <> r')

-- appendK l r k = k (l <> r)
appendK :: Tree a -> Tree a -> (Tree a -> Tree a) -> Tree a
appendK l r k =
  case l of
    Leaf a -> k (Cons (Leaf a) r)
    Node t -> k (Cons (Node t) r)
    Cons ll lr -> k (ll <> (lr <> r))