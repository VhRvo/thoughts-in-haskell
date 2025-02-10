module BreadthFirst.Level where

import BreadthFirst.Tree

breadth :: forall m a b. (Applicative m) => (a -> m b) -> Tree a -> m (Tree b)
breadth f root = head <$> go [root]
  where
    go :: [Tree a] -> m [Tree b]
    go trees =
      let children = childrenOfTrees trees
       in --   values = traverse f (nodes trees)
          if null children
            then pure (Empty <$ trees)
            else -- composeForTrees trees <$> values <*> go children
              -- How to fuse multiple traversal?
              composeChildren
                <$> traverse
                  ( \case
                      Node x _ _ -> Just <$> f x
                      Empty -> pure Nothing
                  )
                  trees
                <*> go children

nodes :: [Tree a] -> [a]
nodes =
  foldr
    ( \element acc ->
        case element of
          Node x _ _ -> x : acc
          Empty -> acc
    )
    []

childrenOfTrees :: [Tree a] -> [Tree a]
childrenOfTrees = concatMap children
  where
    children :: Tree a -> [Tree a]
    children = \case
      Empty -> []
      Node _ left right -> [left, right]

composeForTrees :: [Tree a] -> [b] -> [Tree b] -> [Tree b]
composeForTrees = go
  where
    -- use mapAccumL to refactoring go
    go [] values children = []
    go (Empty : trees) values children = Empty : go trees values children
    go (Node {} : trees) (value : values) (left : right : children) =
      Node value left right : go trees values children

composeChildren :: [Maybe b] -> [Tree b] -> [Tree b]
composeChildren = go
  where
    -- use mapAccumL to refactoring go
    go [] [] = []
    go (Nothing : trees) children = Empty : go trees children
    go (Just x : trees) (left : right : children) =
      Node x left right : go trees children

demo1 :: IO ()
demo1 = do
  breadth print tree1 >>= print


