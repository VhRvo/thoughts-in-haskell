{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}

module MonadFix where

import Control.Monad.Fix
import Data.Semigroup (Max)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromJust)

data RoseTree a = RoseTree a [RoseTree a]
  deriving (Show)

exampleTree :: RoseTree Int
exampleTree = RoseTree 5 [RoseTree 4 [], RoseTree 6 []]

-- >>> pureMax exampleTree
pureMax :: (Ord a) => RoseTree a -> RoseTree (a, a)
pureMax top =
  let (tree', largest) = go largest top
   in tree'
  where
    go :: (Ord a) => a -> RoseTree a -> (RoseTree (a, a), a)
    go largest (RoseTree x []) = (RoseTree (x, largest) [], x)
    go largest (RoseTree x xs) =
      let sub = map (go largest) xs
          (xs', largests) = unzip sub
          !subMaximum = maximum largests
    --    in (RoseTree (x, largest) xs', max x (maximum largests))
       in (RoseTree (x, largest) xs', max x subMaximum)

impureMin :: forall m a b. (MonadFix m, Ord b) => (a -> m b) -> RoseTree a -> m (RoseTree (a, b))
impureMin f top = mdo
  (tree', smallest) <- go smallest top
  pure tree'
  where
    go :: b -> RoseTree a -> m (RoseTree (a, b), b)
    go smallest tree = case tree of
      RoseTree x trees -> do
        value <- f x
        sub <- traverse (go smallest) trees
        let (trees', smallests) = unzip sub
        let smallest' = if null smallests then value else min value (minimum smallests)
        pure (RoseTree (x, smallest) trees', smallest')

budget :: String -> IO Int
budget "Ada" = return 10 -- A struggling startup programmer
budget "Curry" = return 50 -- A big-earner in finance
budget "Dijkstra" = return 20 -- Teaching is the real reward
budget "Howard" = return 5 -- An fragile undergraduate!
budget _ = return 100

inviteTree :: RoseTree String
inviteTree =
  RoseTree
    "Ada"
    [ RoseTree "Dijkstra" [],
      RoseTree "Curry" [RoseTree "Howard" []]
    ]

-- >>> impureMin budget inviteTree
-- RoseTree ("Ada",5) [RoseTree ("Dijkstra",5) [],RoseTree ("Curry",5) [RoseTree ("Howard",5) []]]

fIO :: IO (Int -> Int)
fIO = do
  let fac x = if x == 0 then 1 else x * fac (x - 1)
      tri x = if x == 0 then 0 else x + tri (x - 1)
  b <- readLn
  pure $ if b then fac else tri

-- fIO :: IO (Int -> Int)
-- fIO = do
--   b <- readLn
--   pure $ \x ->
--     if b then fac else tri

fIOf :: IO (Int -> Int) -> IO (Int -> Int)
fIOf fIO' = do
  b <- readLn
  pure $ \x ->
    if b
      then if x == 0 then 1 else x * unsafePerformIO fIO' (x - 1)
      else if x == 0 then 0 else x + unsafePerformIO fIO' (x - 1)

fIOmfix :: IO (Int -> Int)
fIOmfix = mfix gIO'
  where
    gIO' :: (Int -> Int) -> IO (Int -> Int)
    gIO' f = do
        b <- readLn
        pure $ \x ->
            if b
                then if x == 0 then 1 else x * f (x - 1)
                else if x == 0 then 0 else x + f (x - 1)

tuple :: ([Int], Int, Int)
-- tuple = fix $ \(~(a, b, c)) -> ([1, c-5], head a + 2, b * 4)
tuple = fix $ \(a, b, c) -> ([1, c-5], head a + 2, b * 4)
  where
    f (a, b, c) = ([1, c-5], head a + 2, b * 4)

maybeFix :: Maybe Int
-- maybeFix = mfix (const Nothing)
maybeFix = mfix (\x ->
    Just 1)
  where
    -- mfix' f = let x = fromJust (f x) in Just x


