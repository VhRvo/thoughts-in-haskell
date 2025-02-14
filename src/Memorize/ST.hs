module Memorize.ST where

import Control.Monad.ST
import Data.STRef
import Data.HashTable.ST.Basic

-- memorize :: (Eq a, Hashable a) => ((a -> b) -> a -> b) -> ((a -> b) -> a -> b)
-- memorize factorial f x = do
--   r <- readSTRef ref
--   case r of
--     Nothing -> do
--       v <- f x
--       writeSTRef ref (Just v)
--       return v
--     Just v  -> return v

