{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScottEncoding where

import Prelude hiding (concat, curry, either, foldl, foldr, fst, head, length, map, null, snd, tail, take, uncurry, zip, (++))

-- import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

-- const :: forall a b. a -> b -> a
-- const x _ = x

newtype SPair a b = SPair {runPair :: forall c. (a -> b -> c) -> c}

toPair :: forall a b. SPair a b -> (a, b)
toPair sPair = runPair sPair (,)

fromPair :: (a, b) -> SPair a b
fromPair (x, y) = SPair (\eliminator -> eliminator x y)

fst :: forall a b. SPair a b -> a
fst sPair = runPair sPair const

snd :: forall a b. SPair a b -> b
snd sPair = runPair sPair (\_ y -> y)

swap :: forall a b. SPair a b -> SPair b a
swap sPair = runPair sPair (\x y -> fromPair (y, x))

curry :: forall a b c. (SPair a b -> c) -> (a -> b -> c)
curry eliminator x y = eliminator (fromPair (x, y))

uncurry :: forall a b c. (a -> b -> c) -> (SPair a b -> c)
uncurry f sPair = runPair sPair f

newtype SMaybe a = SMaybe {runMaybe :: forall b. b -> (a -> b) -> b}

toMaybe :: forall a. SMaybe a -> Maybe a
toMaybe sMaybe = runMaybe sMaybe Nothing Just

-- maybe1 :: forall a r. r -> (a -> r) -> Maybe a -> r
-- maybe1 nothing just maybe' = case maybe' of
--   Nothing -> nothing
--   Just value -> just value

fromMaybe :: forall a. Maybe a -> SMaybe a
fromMaybe maybe' = SMaybe (\nothing f -> maybe nothing f maybe')

isJust :: forall a. SMaybe a -> Bool
isJust sMaybe = runMaybe sMaybe False (const True)

isNothing :: forall a. SMaybe a -> Bool
isNothing sMaybe = runMaybe sMaybe True (const False)

newtype SEither a b = SEither {runEither :: forall c. (a -> c) -> (b -> c) -> c}

toEither :: forall a b. SEither a b -> Either a b
toEither sEither = runEither sEither Left Right

either :: forall a b r. (a -> r) -> (b -> r) -> Either a b -> r
either left right either' = case either' of
  Left value -> left value
  Right value -> right value

fromEither :: forall a b. Either a b -> SEither a b
fromEither either' = SEither (\left right -> either left right either')

isLeft :: forall a b. SEither a b -> Bool
isLeft sEither = runEither sEither (const True) (const False)

isRight :: forall a b. SEither a b -> Bool
isRight sEither = runEither sEither (const False) (const True)

newtype SList a = SList {runList :: forall b. b -> (a -> SList a -> b) -> b}

empty :: forall a. SList a
empty = SList (\empty' _ -> empty')

toList :: forall a. SList a -> [a]
toList sList = runList sList [] (\head tail -> head : toList tail)

fromList :: [a] -> SList a
fromList list =
  SList
    ( \empty' cons' -> case list of
        [] -> empty'
        x : xs -> cons' x (fromList xs)
    )

cons :: a -> SList a -> SList a
cons head tail = SList (\_ f -> f head tail)

concat :: forall a. SList a -> SList a -> SList a
concat xs ys = runList xs ys (\head tail -> cons head (concat tail ys))

-- concat xs ys =
--   SList
--     ( \empty' cons' ->
--         runList
--           xs
--           (runList ys empty' cons')
--           (\head tail -> cons' head (concat tail ys))
--     )

null :: forall a. SList a -> Bool
null sList = runList sList True (\_ _ -> False)

length :: forall a. SList a -> Int
length sList = runList sList 0 (\_ tail -> 1 + length tail)

map :: forall a b. (a -> b) -> SList a -> SList b
map f sList = runList sList empty (\head tail -> cons (f head) (map f tail))

-- map f sList =
--   SList
--     -- @r r (b -> SList b -> r)
--     ( \empty' cons' ->
--         runList
--           sList
--           -- r
--           empty'
--           -- (a -> SList a -> r)
--           (\head tail -> cons' (f head) (map f tail))
--     )

zip :: forall a b. SList a -> SList b -> SList (SPair a b)
zip as bs =
  --   runList as
  --     empty
  --     ( \headA tailA ->
  --         runList bs
  --           empty
  --           (\headB tailB -> cons (fromPair (headA, headB)) (zip tailA tailB))
  --     )
  --   @r   r      (SPair a b) -> SList (SPair a b) -> r
  SList
    ( \empty' cons' ->
        runList
          as
          empty'
          ( \headA tailA ->
              runList
                bs
                empty'
                (\headB tailB -> cons' (fromPair (headA, headB)) (zip tailA tailB))
          )
    )

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl combine accumulator sList =
  runList sList accumulator (foldl combine . combine accumulator)

-- runList sList accumulator (\head tail -> foldl combine (combine accumulator head) tail)

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr combine empty' sList =
  runList sList empty' (\head tail -> combine head (foldr combine empty' tail))

take :: Int -> SList a -> SList a
take index sList =
  if index == 0
    then empty
    else runList sList empty (\head tail -> cons head (take (index - 1) tail))

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition sList =
  runList
    sList
    (fromPair (empty, empty))
    ( \head tail ->
        runEither
          head
          (\left -> runPair (partition tail) (\first second -> fromPair (cons left first, second)))
          (\right -> runPair (partition tail) (\first second -> fromPair (first, cons right second)))
    )

catMaybes :: SList (SMaybe a) -> SList a
catMaybes sList =
  runList
    sList
    empty
    (\head tail -> runMaybe head (catMaybes tail) (\value -> cons value (catMaybes tail)))
