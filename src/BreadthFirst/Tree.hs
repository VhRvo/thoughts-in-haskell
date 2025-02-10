module BreadthFirst.Tree where

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

isNode :: Tree a -> Bool
isNode = \case
  Empty -> False
  Node {} -> True

tree1 :: Tree Char
tree1 =
  Node
    'a'
    (Node 'b' Empty (Node 'c' Empty Empty))
    (Node 'd' Empty Empty)
