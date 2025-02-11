module Sudoku.Simple where

import Prelude hiding (all)

type Matrix a = [Row a]

type Row a = [a]

type Column a = [a]

type Box a = [a]

type Grid = Matrix Digit

type Digit = Char

digits :: [Char]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = filter valid . completions
  where
    completions :: Grid -> [Grid]
    completions = expand . choices

choices :: Grid -> Matrix [Digit]
choices = fmap . fmap $ choice

choice :: Digit -> [Digit]
choice d
  | blank d = digits
  | otherwise = [d]

-- cartesianProduct :: [Possible a]^m -> Possible ([a]^m)
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct = \case
  [] -> [[]]
  xs : xss -> [x : ys | x <- xs, ys <- yss]
    where
      yss = cartesianProduct xss

-- >>> cartesianProduct [[1], [2], [3]]

-- expand :: [[Possible a]^n]^m -> Possible ([[a]^n]^m)
expand :: Matrix [Digit] -> [Grid]
expand =
  cartesianProduct -- [Possible ([a]^n)]^m -> Possible ([[a]^n]^m)
    . fmap cartesianProduct -- [[Possible a]^n]^m -> [Possible ([a]^n)]^m

-- fmap rows -- Possible (Matrix a) -> Possible [Row a]
--  . expand -- Matrix (Possible a) -> Possible (Matrix a)
-- =
-- expand -- [Row (Possible a)] -> Possible [Row a]
-- . rows -- Matrix (Possible a) -> [Row (Possible a)]

-- fmap columns -- Possible (Matrix a) -> Possible [Column a]
--  . expand -- Matrix (Possible a) -> Possible (Matrix a)
-- =
-- expand -- [Column (Possible a)] -> Possible [Column a]
-- . columns -- Matrix (Possible a) -> [Column (Possible a)]

-- fmap boxes -- Possible (Matrix a) -> Possible [Box a]
--  . expand -- Matrix (Possible a) -> Possible (Matrix a)
-- =
-- expand -- [Box (Possible a)] -> Possible [Box a]
-- . boxes -- Matrix (Possible a) -> [Box (Possible a)]

-- choices

valid :: Grid -> Bool
valid grid =
  all noDuplicates (rows grid)
    && all noDuplicates (columns grid)
    && all noDuplicates (boxes grid)

all :: (a -> Bool) -> [a] -> Bool
all predicate = and . fmap predicate

-- rows :: [[a]^n]^m -> [[a]^n]^m
rows :: Matrix a -> [Row a]
rows = id

-- columns :: [[a]^n]^m -> [[a]^m]^n
columns :: Matrix a -> [Column a]
columns = \case
  [] -> error "unexpected 0*n matrix input"
  [xs] -> fmap pure xs
  xs : xss -> zipWith (:) xs (columns xss)

boxSize :: Int
boxSize = 3

-- group :: [a]^n -> [[a]^3]^(n/3)
group :: [a] -> [[a]]
group = \case
  [] -> []
  xs -> front : group rest
    where
      (front, rest) = splitAt boxSize xs

-- unGroup :: [[a]^n]^m -> [a]^(m*n)
unGroup :: [[a]] -> [a]
unGroup = concat

-- boxes :: [[a]^n]^m -> [[a]^9]^(m*n/9)
boxes :: Matrix a -> [[a]]
boxes =
  fmap unGroup -- [[[a]^f]^e]^(nm/fe) -> [[a]^(fe)]^(nm/fe)
    . unGroup -- [[[[a]^f]^e]^(n/f)]^(m/e) -> [[[a]^f]^e]^(nm/fe)
    . fmap columns -- [ [[ [a]^f ]^(n/f)]^e ]^(m/3) -> [[[[a]^f]^e]^(n/f)]^(m/e)
    . group -- [[[a]^f]^(n/f)]^m -> [[ [[a]^f]^(n/f) ]^e]^(m/3)
    . fmap group -- [[a]^n]^m -> [ [[a]^f]^(n/f) ]^m

noDuplicates :: (Eq a) => [a] -> Bool
noDuplicates = \case
  [] -> True
  x : xs -> notElem x xs && noDuplicates xs

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = fmap (remove fixed) row
  where
    fixed = [d | [d] <- row]

remove :: [Digit] -> [Digit] -> [Digit]
remove _ [x] = [x]
remove ds xs = filter (`notElem` ds) xs

-- >>> pruneRow [[6], [1, 2], [3], [1, 3, 4], [5, 6]]
-- [[6], [1, 2], [3], [1, 4], [5]]
-- >>> pruneRow [[6], [3, 6], [3], [1, 3, 4], [4]]
-- [[6], [], [3], [1], [4]]
