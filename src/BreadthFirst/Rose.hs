-- https://patternsinfp.wordpress.com/2015/03/05/breadth-first-traversal/
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module BreadthFirst.Rose where

import Control.Arrow ((&&&))
import Data.Functor.Compose

data Tree a = Tree {root :: a, children :: [Tree a]}
  deriving (Functor)

foldrTree :: (a -> r -> r) -> (r -> r -> r) -> r -> Tree a -> r
foldrTree f g a (Tree root children) =
  f root (foldr (g . foldrTree f g a) a children)
  -- f root (foldr g a . fmap (foldrTree f g a) $ children)

class FoldTree f where
    foldTree :: (a -> r -> r) -> (r -> r -> r) -> r -> f a -> r

instance FoldTree Tree where
  foldTree :: (a -> r -> r) -> (r -> r -> r) -> r -> Tree a -> r
  foldTree f g a (Tree root children) = f root (foldTree f g a (Compose children))

instance FoldTree (Compose [] Tree) where
  foldTree :: (a -> r -> r) -> (r -> r -> r) -> r -> (Compose [] Tree) a -> r
  foldTree f g a (Compose trees) = foldr (g . foldTree f g a) a trees

shape :: (Functor f) => f a -> f ()
shape = fmap (const ())

levels :: Tree a -> [[a]]
-- levels (Tree root children) = [root] : foldr (longZipWith (<>)) [] (fmap levels children)
levels (Tree root children) = [root] : foldr (longZipWith (<>) . levels) [] children

longZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
longZipWith combine = go
  where
    go [] rest = rest
    go rest [] = rest
    go (x : xs) (y : ys) = combine x y : go xs ys

breadthFirst :: Tree a -> [a]
breadthFirst = concat . levels

instance Foldable Tree where
  foldMap f = foldMap f . breadthFirst

relabel :: (Tree (), [[a]]) -> (Tree a, [[a]])
relabel (Tree _ children, (x : xs) : xss) =
  (Tree x us, xs : yss)
  where
    (us, yss) = relabels (children, xss)

relabels :: ([Tree ()], [[a]]) -> ([Tree a], [[a]])
relabels ([], xss) = ([], xss)
relabels (t : ts, xss) = (u : us, zss)
  where
    (u, yss) = relabel (t, xss)
    (us, zss) = relabels (ts, yss)

bfLabel :: Tree () -> [a] -> Tree a
bfLabel tree xs = u
  where
    (u, xss) = relabel (tree, xs : xss)

{-
relabel (t, xss) = (u, yss)
<=>
shape u = shape t && longZipWith (<>) (levels u) yss = xSS
-}

{-
relabel (shape tree, levels tree) = (tree, replicate (depth tree) [])
-}

split :: Tree a -> (Tree (), [[a]])
split = shape &&& levels

combine :: Tree () -> [[a]] -> Tree a
combine u xss = fst (relabel (u, xss))

instance Traversable Tree where
  -- traverse f :: t a -> m (t b)
  -- traverse (traverse f) :: t (t a) -> m (t (t b))
  --   traverse f tree = combine (shape tree) <$> traverse (traverse f) (levels tree)
  traverse f tree = bfLabel (shape tree) <$> traverse f (breadthFirst tree)
