module Infinite.Torus where

import Data.List hiding (zipWith4)

data Torus a
  = Cell {elem :: a, up, down, left, right :: Torus a}

type Matrix a = [[a]]

-- x = head (head xss)
-- map (map elem) xss = ass
-- map (map up) xss = rotDown xss
-- map (map down) xss = rotUp xss
-- map (map left) xss = rotRight xss
-- map (map right) xss = rotLeft xss
-- xss = zipWithMatrix (map (map elem) xss) (map (map up) xss) (map (map down) xss) (map (map left) xss) (map (map right) xss)

rotDown :: [a] -> [a]
rotDown xss = tail xss <> [head xss]

rotUp :: [a] -> [a]
rotUp xss = [last xss] <> init xss

rotLeft :: Matrix a -> Matrix a
rotLeft = map rotUp

rotRight :: Matrix a -> Matrix a
rotRight = map rotDown


zipWithMatrix :: Matrix a -> Matrix (Torus a) -> Matrix (Torus a) -> Matrix (Torus a) -> Matrix (Torus a) -> Matrix (Torus a)
zipWithMatrix = zipWith4 (zipWith4 Cell)
-- zipWithMatrix (as:ass) ~(ups:upss) ~(downs:downss) ~(lefts:leftss) ~(rights:rightss) =
--     zipWith4 Cell as ups downs lefts rights : zipWithMatrix ass upss downss leftss rightss
-- zipWithMatrix [] _ _ _ _ = []

zipWith4 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith4 f (a:as) ~(up:ups) ~(down:downs) ~(left:lefts) ~(right:rights) =
    f a up down left right : zipWith4 f as ups downs lefts rights
zipWith4 f _ _ _ _ _ = []

mkTorus :: Matrix a -> Torus a
mkTorus ass =
    if all null ass
        then error "torus must have at least one element"
        else head (head xss)
  where
    xss = zipWithMatrix ass (rotDown xss) (rotUp xss) (rotRight xss) (rotLeft xss)
    -- x = head (head xss)
    -- map (map elem) xss = ass
    -- map (map up) xss = rotDown xss
    -- map (map down) xss = rotUp xss
    -- map (map left) xss = rotRight xss
    -- map (map right) xss = rotLeft xss
    -- xss = zipWithMatrix (map (map elem) xss) (map (map up) xss) (map (map down) xss) (map (map left) xss) (map (map right) xss)

takeRight :: Int -> Torus a -> [a]
takeRight 0 _ = []
takeRight n (Cell x _ _ _ right) = x : takeRight (n - 1) right

takeDown :: Int -> Torus a -> [Torus a]
takeDown 0 _ = []
takeDown n cell@(Cell _ _ down _ _) = cell : takeDown (n - 1) down

takeMatrix :: Int -> Int -> Torus a -> [[a]]
takeMatrix height width cell = takeRight width <$> takeDown height cell

torus = mkTorus @Int [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
matrix = takeMatrix 2 3 torus


