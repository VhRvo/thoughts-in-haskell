module FoldrDropWhile where

import Prelude hiding (dropWhile)

-- how to use foldr to implement dropWhile
split :: [a] -> [[a]]
split = foldr (\x acc -> (x : head acc) : acc) [[]]

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' predicate = foldr (\(x : xs) acc -> if predicate x then acc else xs) [] . split

-- >>> dropWhile (<5) [1..10]

-- dWRec :: (a -> Bool) -> [a] -> [a]
-- dWRec predicate = id
-- dWRec predicate = \list@(head:tail) ->
--     if predicate head
--         then dWRec predicate tail
--         else list

dW :: (a -> Bool) -> [a] -> [a]
dW predicate list' =
  foldr
    ( \_ acc list -> case list of
        [] -> []
        x : xs ->
          if predicate x
            then acc xs
            else list
    )
    id
    list'
    list'

-- dW' :: (a
dWNonRec :: ((a -> Bool) -> [a] -> [a]) -> (a -> Bool) -> [a] -> [a]
dWNonRec recurse predicate [] = []
dWNonRec recurse predicate list@(head : tail) =
  if predicate head
    then recurse predicate tail
    else list
