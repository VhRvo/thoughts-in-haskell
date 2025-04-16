module Memorize.YCombinator where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

y1 :: (a -> a) -> a
y1 f =
    -- f (y1 f)
    let knot = f knot
     in knot

y2 :: ((a -> b) -> (a -> b)) -> (a -> b)
y2 f =
    -- f (\x -> y2 f x)
    let knot x = f knot x
     in knot

y3 ::
  ((a -> state -> (state, b)) -> (a -> state -> (state, b))) ->
  (a -> state -> (state, b))
y3 f =
    -- f (\x state -> y3 f x state)
    let knot x state = f knot x state
     in knot

-- y: memo
y4 ::
  (Ord a) =>
  ( (a -> Map a b -> (Map a b, b)) ->
    (a -> Map a b -> (Map a b, b))
  ) ->
  (a -> Map a b -> (Map a b, b))
y4 f =
--   f
--     ( \x memo ->
--         case Map.lookup x memo of
--           Nothing ->
--             let (memo', result) = y4 f x memo
--              in (Map.insert x result memo', result)
--           Just result -> (memo, result)
--     )
  let knot x memo =
        case Map.lookup x memo of
            Nothing ->
                let (memo', result) = f knot x memo
                 in (Map.insert x result memo', result)
            Just result -> (memo, result)
   in knot


-- y: key
y5 ::
  (Ord k) =>
  (a -> k) ->
  ( (a -> Map k b -> (Map k b, b)) ->
    (a -> Map k b -> (Map k b, b))
  ) ->
  (a -> Map k b -> (Map k b, b))
y5 key f =
  f
    ( \x memo ->
        let keyX = key x
         in case Map.lookup keyX memo of
              Nothing ->
                let (memo', result) = y5 key f x memo
                 in (Map.insert keyX result memo', result)
              Just result -> (memo, result)
    )

-- y: multiple tables
y6 ::
  (Ord k) =>
  (a -> k, tables -> Map k b, Map k b -> tables -> tables) ->
  ( (a -> tables -> (tables, b)) ->
    (a -> tables -> (tables, b))
  ) ->
  (a -> tables -> (tables, b))
y6 (key, get, set) f =
  f
    ( \x tables ->
        let
          keyX = key x
          table = get tables
         in
          case Map.lookup keyX table of
            Nothing ->
              let (tables', result) = y6 (key, get, set) f x tables
               in (set (Map.insert keyX result table) tables', result)
            Just result ->
              (tables, result)
    )

-- y: cps
y7 ::
  (Ord k) =>
  (a -> k, tables -> Map k b, Map k b -> tables -> tables) ->
  ( (a -> tables -> ((tables, b) -> c) -> c) ->
    (a -> tables -> ((tables, b) -> c) -> c)
  ) ->
  (a -> tables -> ((tables, b) -> c) -> c)
y7 (key, get, set) f =
  f
    ( \x tables k ->
        let
          keyX = key x
          table = get tables
         in
          case Map.lookup keyX table of
            Nothing ->
              y7 (key, get, set) f x tables $ \(tables', result) ->
                k (set (Map.insert keyX result table) tables', result)
            Just result ->
              k (tables, result)
    )

-- y: cps
y8 ::
  (Ord k) =>
  (a -> k, tables -> Map k b, Map k b -> tables -> tables) ->
  ( (a -> tables -> (tables -> b -> c) -> c) ->
    (a -> tables -> (tables -> b -> c) -> c)
  ) ->
  (a -> tables -> (tables -> b -> c) -> c)
y8 (key, get, set) f =
  f
    ( \x tables k ->
        let
          keyX = key x
          table = get tables
         in
          case Map.lookup keyX table of
            Nothing ->
              y8 (key, get, set) f x tables $ \tables' result ->
                k (set (Map.insert keyX result table) tables') result
            Just result ->
              k tables result
    )

-- y: cps
y9 ::
  (Ord k) =>
  (a -> k, tables -> Map k b, Map k b -> tables -> tables) ->
  ( (a -> tables -> (tables -> b -> c) -> c) ->
    (a -> tables -> (tables -> b -> c) -> c)
  ) ->
  (a -> tables -> (tables -> b -> c) -> c)
y9 (key, get, set) f =
  f
    ( \x tables k ->
        let
          keyX = key x
          table = get tables
         in
          case Map.lookup keyX table of
            Nothing ->
              y9 (key, get, set) f x tables $ \tables' code ->
                let result = code
                 in k (set (Map.insert keyX result table) tables') result
            Just result ->
              k tables result
    )
