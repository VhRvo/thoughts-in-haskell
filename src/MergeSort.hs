module MergeSort where

import Data.Bifunctor

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] l2 = l2
merge l1 [] = l1
merge l1@(x : xs) l2@(y : ys) =
  case compare x y of
    LT -> x : merge xs l2
    GT -> y : merge l1 ys
    EQ -> x : y : merge xs ys

split :: [a] -> ([a], [a])
split list = case list of
  [] -> (list, [])
  [_] -> (list, [])
  x : y : zs ->
    let (xs, ys) = split zs
     in (x : xs, y : ys)

-- You iterate over the list twice simultaneously:
-- by skipping two elements at once and by skipping just one.
-- The former finishes right when the latter is in the middle of the list,
-- so you get your latter half of the list (without recreating it,
-- which would be unnecessary allocations) and can recreate the former one.
halve :: [a] -> ([a], [a])
halve xs0 =
  go xs0 xs0
  where
    go (_ : _ : xsFast) (x : xsSlow) = first (x :) $ go xsFast xsSlow
    go _ xsSlow = ([], xsSlow)

mergeSort :: (Ord a) => [a] -> [a]
mergeSort list = case list of
  [] -> list
  [_] -> list
  _ ->
    let (xs, ys) = split list
     in merge (mergeSort xs) (mergeSort ys)

--- >> mergeSort [5, 4, 1, 2, 3]
