module Memorize.Pure where

import Data.Map
import Control.Monad.State
-- import Data.Hashable

-- memorize :: (Ord a, Monad m) =>
--   (((a -> b) -> a -> b) -> a -> b) ->
--   (((a -> m b) -> a -> m b) -> a -> m b)

-- memorize :: (Ord a, Monad m) =>
--   (((a -> b) -> a -> b) -> a -> b) ->
--   ((a -> State (Map a b) b) -> a -> State (Map a b) b) -> a -> State (Map a b) b

-- memorize :: (Ord a, Monad m) =>
--   (((a -> b) -> a -> b) -> a -> b) ->
--   ((a -> Map a b -> (b, Map a b)) -> a -> Map a b -> (b, Map a b)) -> a -> Map a b -> (b, Map a b)
-- memorize = undefined

-- memorize functional f x = case lookup x memo of
--   Nothing -> let y = f x in insert x y memo >> return y
--   Just y   -> return y
