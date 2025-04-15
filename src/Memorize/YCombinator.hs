module Memorize.YCombinator where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- import

y1 :: (a -> a) -> a
y1 f = f (y1 f)

y2 :: ((a -> b) -> (a -> b)) -> (a -> b)
y2 f = f (\x -> y2 f x)

y3 :: ((a -> state -> (state, b)) -> (a -> state -> (state, b))) -> 
    (a -> state -> (state, b))
y3 f = f (\x state -> y3 f x state)

y4 :: (Ord a) => 
((a -> Map a b -> (Map a b, b)) -> (a -> Map a b -> (Map a b, b))) -> (a -> Map a b -> (Map a b, b))
y4 f =
  f
    ( \x memo ->
        case Map.lookup x memo of
          Nothing ->
            let (memo', result) = y4 f x memo
             in (Map.insert x result memo', result)
          Just result -> (memo, result)
    )

y5 :: (Ord k) => (a -> key) -> 
    ((a -> Map k b -> (Map k b, b)) -> (a -> Map k b -> (Map k b, b))) -> 
        (a -> Map k b -> (Map k b, b))
y5 key f = f (y5 key f)
