module Memorize.ST where

-- import Control.Monad.ST
-- import Data.HashTable.ST.Basic
-- import Data.STRef

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

memorize :: (Ord a) => ((a -> b) -> (a -> b)) -> ((a -> Map a b -> (Map a b, b)) -> (a -> Map a b -> (Map a b, b)))
memorize factorial f x memo =
  undefined

--   r <- readSTRef ref
--   case r of
--     Nothing -> do
--       v <- f x
--       writeSTRef ref (Just v)
--       return v
--     Just v  -> return v
