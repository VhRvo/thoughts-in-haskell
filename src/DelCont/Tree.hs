module DelCont.Tree where

import Control.Monad.CC -- (MonadDelimitedCont, reset, shift)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromJust)
import System.Random (randomRIO)

-- import Control.Monad.CC (reset)

data Tree a
  = Leaf
  | Branch a (Tree a) (Tree a)
  deriving (Show)

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton a = Branch a Leaf Leaf

insert :: (Ord a) => a -> Tree a -> Tree a
insert b Leaf = Branch b Leaf Leaf
insert b (Branch a l r)
  | b < a = Branch a (insert b l) r
  | otherwise = Branch a l (insert b r)

fold :: (a -> b -> b -> b) -> b -> Tree a -> b
fold _ z Leaf = z
fold f z (Branch x l r) = f x (fold f z l) (fold f z r)

for :: (Monad m) => Tree a -> (a -> m b) -> m ()
for t f = fold (\x l r -> l >> f x >> r) (pure ()) t

data Iterator m a
  = Done
  | Current a (m (Iterator m a))

begin :: (MonadDelimitedCont p s m) => Tree a -> m (Iterator m a)
begin tree = reset $ \p ->
  for
    tree
    ( \element ->
        shift p (\k -> pure (Current element (k $ pure ())))
    )
    >> pure Done

current :: Iterator m a -> Maybe a
current = \case
  Done -> Nothing
  Current x _ -> Just x

next :: (Monad m) => Iterator m a -> m (Iterator m a)
next = \case
  Done -> pure Done
  Current _ i -> i

finished :: Iterator m a -> Bool
finished = \case
  Done -> True
  _ -> False

main :: IO ()
main = runCCT $ do
  t <- randomTree 10
  i <- begin t
  doStuff i
  where
    doStuff i
      | finished i = pure ()
      | otherwise = do
          i' <- next i
          i'' <- next i' -- this is ignored
          liftIO $ print (fromJust $ current i :: Int)
          doStuff i'

randomTree n = rt empty n
  where
    rt t 0 = pure t
    rt t n = do
      r <- liftIO (randomRIO (1, 100))
      rt (insert r t) (n - 1)
