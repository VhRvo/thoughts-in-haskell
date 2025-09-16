module BreadthFirst.Tree where

import Control.Monad.State.Strict

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

bfNumber :: Tree a -> Tree Int
bfNumber tree = evalState (go [tree]) 0
  where
    go :: [Tree a] -> State Int (Tree Int)
    go [Empty] = pure Empty
    go [(Node _ left right)] = undefined
