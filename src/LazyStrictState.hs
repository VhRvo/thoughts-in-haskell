{-# LANGUAGE BangPatterns #-}

module LazyStrictState where

import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict

lazyFoo :: Lazy.State () [Int]
-- lazyFoo = traverse pure [1..]
lazyFoo = pure [1 ..]

lazyFooResult = take 5 (Lazy.evalState lazyFoo ())

strictFoo :: Strict.State () [Int]
strictFoo = traverse pure [1 ..]

strictFooResult = take 5 (Strict.evalState strictFoo ())

infinite :: Strict.State () [Int]
infinite = (:) <$> pure 0 <*> pure [1 ..]

infiniteResult = take 5 (Strict.evalState infinite ())

-- lazyFoo' :: Lazy.State () [Int]
-- lazyFoo

-- or' True _ = True
-- or' _ True = True
-- or' _ _ = False

-- data Maybe' a = Just' !a | Nothing'

-- instance Show (Maybe' a) where
--     show :: Maybe' a -> String
--     show = \case
--       Just' a -> "Just'"
--       Nothing' -> "Nothing'"

-- f :: Maybe Bool -> Bool -> Int
-- f _    False = 1
-- f (Just _) False = 2
-- f _    _     = 3

-- test = f (Just undefined)

-- sum1 :: Integer
-- sum1 = foldr (+) 0 [1..100000000]
-- sum1' :: Integer
-- sum1' = foldr (\(!a) (!b) -> a + b) 0 [1..100000000]

-- t :: Integer
-- t = case undefined of
--   _ -> 1
