{-# LANGUAGE ScopedTypeVariables #-}

module BreadthFirst.Context where

import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (unsnoc)
import Data.Maybe (fromMaybe)
import Prelude hiding (sequence, tail)
import BreadthFirst.Tree

breadth' :: forall m a. (Applicative m) => (a -> m ()) -> Tree a -> m ()
breadth' f root = go [root]
  where
    go :: [Tree a] -> m ()
    go = \case
      [] -> pure ()
      Empty : sequence -> go sequence
      Node x left right : sequence ->
        f x
          *> go (sequence <> [left, right])

-- breadth :: forall m a b. (Applicative m) => (a -> m b) -> Tree a -> m (Tree b)
-- breadth f root = head <$> go [root]
--   where
--     go :: [Tree a] -> m [Tree b]
--     go = \case
--       [] -> pure []
--       Empty : sequence -> (:) Empty <$> go sequence
--       Node x left right : sequence ->
--         (\y (sequence', left', right') -> Node y left' right' : sequence')
--           <$> f x
--           <*> (unTwoSnoc <$> go (sequence <> [left, right]))

breadth :: forall m a b. (Applicative m) => (a -> m b) -> Tree a -> m (Tree b)
breadth f root = head <$> go [root]
  where
    go :: [Tree a] -> m [Tree b]
    go = \case
      [] -> pure []
      Empty : sequence -> (<> [Empty]) <$> go sequence
      Node x left right : sequence ->
        (\y (right', left', sequence') -> Node y left' right' : sequence')
          <$> f x
          <*> (unTwoCons <$> go (sequence <> [left, right]))

unTwoCons :: [a] -> (a, a, [a])
unTwoCons (first : second : tail) = (first, second, tail)
unTwoCons _ = error "unexpected list"

unTwoSnoc :: [a] -> ([a], a, a)
unTwoSnoc xs = fromMaybe (error "unexpected list") $ do
  (init, last) <- unsnoc xs
  (init', secondToLast) <- unsnoc init
  pure (init', secondToLast, last)

-- go :: Tree a -> [Tree a] -> m ()
-- go tree sequence = case tree of
--   Empty -> case sequence of
--     [] -> pure ()
--     head : tail -> go head tail
--   Node x left right ->
--     f x
--       *> case sequence of
--         [] -> go left [right]
--         head : tail -> go head (tail <> [left, right])

-- >>> demo1
demo1 :: IO ()
demo1 = breadth' print tree1

demo2 :: IO ()
demo2 = do
  ref <- newIORef @Int 0
  breadth'
    ( \_ -> do
        readIORef ref >>= print
        modifyIORef ref (+ 1)
    )
    tree1

demo3 :: IO ()
demo3 = do
  ref <- newIORef @Int 0
  tree <-
    breadth
      ( \_ -> do
          value <- readIORef ref
          print value
          modifyIORef ref (+ 1)
          pure value
      )
      tree1
  print tree
