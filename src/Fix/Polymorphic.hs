-- {-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

module Fix.Polymorphic where

data Nested a = Nil | a :< Nested [a]

infixr 5 :<

nested :: Nested Int
nested = 1 :< [2, 3] :< [[3, 4], [5]] :< Nil

fix :: forall a. (a -> a) -> a
fix f =
  let x = f x
   in x

polymorphicFix ::
  forall f. ((forall a. f a) -> (forall a. f a)) -> (forall a. f a)
polymorphicFix f =
  let
    x :: forall a. f a
    x = f x
   in
    x

lengthNestedF :: (Nested [a] -> Int) -> Nested a -> Int
lengthNestedF recurse ns = case ns of
  Nil -> 0
  _ :< nns -> 1 + recurse nns

lengthNestedF2 :: (forall a. Nested a -> Int) -> Nested a -> Int
lengthNestedF2 recurse ns = case ns of
  Nil -> 0
  _ :< nns -> 1 + recurse nns

-- lengthNestedF2 :: forall a. (forall a. Nested a -> Int) -> Nested a -> Int
-- lengthNestedF2 recurse ns = case ns of
--   Nil      -> 0
--   _ :< nns -> 1 + recurse @[a] nns

newtype NestedFunction a
  = NestedFunction {unNestedFunction :: Nested a -> Int}

newtype Forall f = Forall {unForall :: forall a. f a}

lengthNested2 :: forall a. Nested a -> Int
lengthNested2 = unNestedFunction (polymorphicFix (\x -> NestedFunction (lengthNestedF2 (unNestedFunction x))))

-- lengthNested2 = unNestedFunction (polymorphicFix (NestedFunction . lengthNestedF2 . unNestedFunction))

-- lengthNested2' :: forall a. Nested a -> Int
-- lengthNested2' = polymorphicFix lengthNestedF2
-- lengthNested2' =
--     let x = lengthNestedF2 x
--      in x

fix' :: forall f. ((forall a. f a) -> (forall a. f a)) -> forall a. f a
fix' f = unForall (fix (\x -> Forall (f (unForall x))))

-- fix'' :: forall f. (forall a. (forall a. f a) -> f a) -> forall a. f a
-- fix'' f = fix @(forall a. f a) f
-- fix'' f = fix f
