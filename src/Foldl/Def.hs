module Foldl.Def where

import Prelude (flip, id, ($), (<>))

foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl combine intial = foldr (flip combine) initial . reverse

-- foldl combine initial []
-- --  = foldr (flip combine) initial . reverse []
-- --  = foldr (flip combine) initial []
--  = initial
-- foldl combine initial (x:xs)
-- --   = foldr (flip combine) initial . reverse $ (x:xs)
-- --   = foldr (flip combine) initial $ reverse xs <> [x]
-- --   = foldr (flip combine) (flip combine x initial) $ reverse xs
--   = foldr (flip combine) (combine initial x) $ reverse xs

-- foldl combine = go
--   where
--     go acc []     = acc
--     go acc (x:xs) = go (combine acc x) xs

-- foldl combine initial xs = go xs initial
--   where
--     go []     acc = acc
--     go (x:xs) acc = go xs (combine acc x)

-- foldl combine initial xs = go xs initial
--   where
--     go []     = id
--     go (x:xs) = \acc' -> go xs (combine acc' x)

-- foldl combine initial xs = go xs initial
--   where
--     go []     = id
--     go (x:xs) = (\x acc acc' -> acc (combine acc' x)) x (go xs)

foldl combine initial xs = go xs initial
  where
    go = foldr (\x acc acc' -> acc (combine acc' x)) id

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr combine initial = go
  where
    go [] = initial
    go (x : xs) = combine x (go xs)

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs <> [x]
