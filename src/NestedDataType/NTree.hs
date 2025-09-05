{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module NestedDataType.NTree where

data Two a = Two a a
  deriving (Eq, Ord, Show)
  deriving (Functor, Applicative, Foldable, Traversable)

swap :: Two a -> Two a
swap (Two x y) = Two y x

data Tree a
  = Leaf a
  | Node (Two (Tree a))
  deriving (Eq, Ord, Show)
  deriving (Functor, Applicative, Foldable, Traversable)

data NTree a
  = NLeaf a
  | NNode (NTree (Two a))
  deriving (Eq, Ord, Show)
  deriving (Functor, Applicative, Foldable, Traversable)

ntmap :: (a -> b) -> NTree a -> NTree b
ntmap f (NLeaf x) = NLeaf (f x)
ntmap f (NNode trees) = NNode (ntmap (fmap f) trees)

-- >>> pure 1 :: Two Int
-- /Users/zhangyongzhuo/Desktop/Workplace/Learning/thoughts-in-haskell/src/NestedDataType/NTree.hs:10:22-32: No instance nor default method for class operation pure

invertTree :: forall a. Tree a -> Tree a
invertTree = \case
  Leaf x -> Leaf x
  Node trees -> Node (swap (fmap invertTree trees))

invertNTree :: forall a. NTree a -> NTree a
invertNTree = go id
  where
    go :: forall a. (a -> a) -> NTree a -> NTree a
    go f (NLeaf x) = NLeaf (f x)
    go f (NNode trees) = NNode (go (swap . fmap f) (invertNTree trees))

replicateTree :: a -> Int -> Tree a
replicateTree x = go
  where
    go 0 = Leaf x
    go m | m < 0 = error "invalid argument to replicateTree"
    go m = Node (Two tree tree)
      where
        tree = go (m - 1)

replicateNTree :: forall a. a -> Int -> NTree a
replicateNTree x = go
  where
    go :: Int -> NTree a
    go 0 = NLeaf x
    go m | m < 0 = error "invalid argument to replicateNTree"
    go m = NNode tree'
      where
        tree = go (m - 1)
        tree' = ntmap (\x -> Two x x) tree

replicateNTree' :: forall a. a -> Int -> NTree a
replicateNTree' = go
  where
    go :: forall a. a -> Int -> NTree a
    go x 0 = NLeaf x
    go _ m | m < 0 = error "invalid argument to replicateNTree"
    go x m = NNode (go (Two x x) (m - 1))

toNTree :: forall a. Tree a -> Maybe (NTree a)
toNTree = go
  where
    go :: forall a. Tree a -> Maybe (NTree a)
    go (Leaf x) = Just (NLeaf x)
    go (Node (Two left right)) = do
      left' <- go left
      right' <- go right
      merge left' right'

merge :: forall a. NTree a -> NTree a -> Maybe (NTree a)
merge = go
  where
    go :: forall a. NTree a -> NTree a -> Maybe (NTree a)
    go (NLeaf left) (NLeaf right) = Just (NNode (NLeaf (Two left right)))
    go (NNode left) (NNode right) = NNode <$> go left right
    go _ _ = Nothing

fromNTree :: forall a. NTree a -> Tree a
fromNTree = go
  where
    go :: forall a. NTree a -> Tree a
    go (NLeaf x) = Leaf x
    go (NNode trees) = unwrap (go trees)
    unwrap :: forall a. Tree (Two a) -> Tree a
    -- unwrap (Leaf (Two x y)) = Node (Two (Leaf x) (Leaf y))
    -- unwrap (Node (Two left right)) = Node (Two (unwrap left) (unwrap right))
    unwrap (Leaf leaf) = Node (Leaf <$> leaf)
    unwrap (Node trees) = Node (unwrap <$> trees)

fromNTree' :: forall a. NTree a -> Tree a
fromNTree' = go
  where
    go :: forall a. NTree a -> Tree a
    go (NLeaf x) = Leaf x
    go (NNode trees) = Node (go <$> split trees)
    split :: forall a. NTree (Two a) -> Two (NTree a)
    split (NLeaf leaf) = NLeaf <$> leaf
    split (NNode trees) = NNode <$> split trees

-- >>> fromNTree (replicateNTree 'c' 3)
-- Node (Two (Node (Two (Node (Two (Leaf 'c') (Leaf 'c'))) (Node (Two (Leaf 'c') (Leaf 'c'))))) (Node (Two (Node (Two (Leaf 'c') (Leaf 'c'))) (Node (Two (Leaf 'c') (Leaf 'c'))))))
-- >>> fromNTree' (replicateNTree 'c' 3)
-- Node (Two (Node (Two (Node (Two (Leaf 'c') (Leaf 'c'))) (Node (Two (Leaf 'c') (Leaf 'c'))))) (Node (Two (Node (Two (Leaf 'c') (Leaf 'c'))) (Node (Two (Leaf 'c') (Leaf 'c'))))))

-- >>> replicateNTree 'c' 3
-- NNode (NNode (NNode (NLeaf (Two (Two (Two 'c' 'c') (Two 'c' 'c')) (Two (Two 'c' 'c') (Two 'c' 'c'))))))
-- >>> replicateNTree' 'c' 3
-- NNode (NNode (NNode (NLeaf (Two (Two (Two 'c' 'c') (Two 'c' 'c')) (Two (Two 'c' 'c') (Two 'c' 'c'))))))
