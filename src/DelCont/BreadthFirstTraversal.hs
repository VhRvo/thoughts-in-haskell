module DelCont.BreadthFirstTraversal where

-- data Tree a
--     = Node a (Tree a) (Tree a)
--     | Leaf a

-- tree = Node 1 (Node 2 (Leaf 3) (Node 4 (Leaf 5) (Leaf 6))) (Node 7 (Node 8 (Leaf 9) (Leaf 10)) (Leaf 11))

-- toList = \case
--     Leaf i -> [i]
--     Node a t1 t2 -> a : toList t1 <> toList t2

-- visit :: MonadDelimitedCont p s m => p [a] -> Tree a -> m ()
-- visit p = visit'
--   where
--     visit' = \case
--         Leaf i -> control p $ \k -> (i:) `liftM` k (pure ())
--         Node i t1 t2 -> control p $ \k -> do
--             a <- k (return ())
--             visit' t2
--             visit' t1
--             (i:) `liftM` pure a

-- bf :: MonadDelimitedCont p s m => Tree a -> m [a]
-- bf t = reset $ \p -> visit p t >> pure []
