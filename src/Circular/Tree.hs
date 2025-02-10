{-# LANGUAGE BangPatterns #-}

module Circular.Tree where

import Data.List (sort)

trace :: (a -> c -> (b, c)) -> a -> b
trace f a = b
  where
    (b, !c) = f a c

data Tree
  = Tip Int
  | Fork Tree Tree
  deriving (Eq, Ord, Show)

size :: Tree -> Int
size (Tip _) = 1
size (Fork left right) = size left + size right

flatten :: Tree -> [Int] -> [Int]
flatten (Tip x) rest = x : rest
flatten (Fork left right) rest = flatten left (flatten right rest)

replace :: Tree -> [Int] -> Tree
replace (Tip _) [x] = Tip x
replace (Fork left right) rest =
  Fork
    (replace left (take (size left) rest))
    (replace right (drop (size left) rest))

-- specification
-- replace1 tree list =
--   ( replace tree (take (size tree) list),
--     drop (size tree) list
--   )

transform' :: Tree -> Tree
transform' tree = replace tree (sort (flatten tree []))

-- specification
-- transform tree sorted flattened =
--   ( replace tree (take (size tree) sorted),
--     drop (size tree) sorted,
--     flatten tree flattened
--   )

transform1 (Tip x) sorted flattened =
  ( Tip (head sorted),
    tail sorted,
    x : flattened
  )
transform1 (Fork left right) sorted flattened =
    -- combine (transform1 left) (transform2 right)
  --   ( replace (Fork left right) (take (size left + size right) sorted),
  --     drop (size left + size right) sorted,
  --     flatten (Fork left right) flattened
  --   ) ==
  --   ( Fork
  --      (replace left (take (size left) (take (size left + size right) sorted)))
  --      (replace right (drop (size left) (take (size left + size right) sorted))),
  --     drop (size left + size right) sorted,
  --     flatten left (flatten right flattened)
  --   ) ==
  --   ( Fork
  --      (replace left (take (size left) sorted)
  --      (replace right (take (size right) (drop (size left) sorted))),
  --     drop (size right) (drop (size left) sorted),
  --     flatten left (flatten right flattened)
  --   ) ==
  let (left', sorted', flattened'') =
        transform1 left sorted flattened'
      -- = ( replace left (take (size left) sorted),
      --   drop (size left) sorted,
      --   flatten left flattened'
      -- )
      (right', sorted'', flattened') =
        transform1 right sorted' flattened
   in -- = ( replace right (take (size right) sorted'),
      --   drop (size right) sorted',
      --   flatten right flattened
      -- )
      ( Fork left' right',
        sorted'', -- drop (size right) (drop (size left) sorted)
        flattened'' -- flatten left (flatten right flattened)
      )

-- combine (left', sorted', flattened'') (right', sorted'', flattened') =
--   (Fork left', right', sorted'', flattened'')

-- specification
-- transform tree sorted flattened =
--   let (tree, rest) = replace tree sorted
--    in ( tree, -- fst (replace tree sorted)
--         rest, -- snd (replace tree sorted)
--         flatten tree flattened
--       )
-- = (replace tree sorted, flatten tree flattened)

-- Why not use the following specification? Just for efficient reason?
-- transform tree sorted flattened =
--   ( replace tree sorted,
--     flatten tree flattened
--   )
