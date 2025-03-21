{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Trie.TrieMap where

import Data.Bifunctor (Bifunctor (first))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Prelude hiding (Traversable (..), lookup)
import Prelude qualified as P

-- import Data.Text (Text)
-- import Data.Text qualified as T

data TrieMap k v
  = -- = Node { file :: Map Text a, directory :: Map Text (TrieMap a) }
  Node {value :: Maybe v, children :: Map k (TrieMap k v)}
  deriving (Show)

empty :: TrieMap k v
empty = Node Nothing Map.empty

alter :: (Ord k) => (Maybe a -> Maybe a) -> [k] -> TrieMap k a -> TrieMap k a
alter f = go
  where
    go prefixes trie = case prefixes of
      [] -> trie {value = f (value trie)}
      key : restPrefixes ->
        trie
          { children =
              Map.alter
                ( Just . \case
                    Nothing -> alterFromEmpty f restPrefixes
                    Just matched -> go restPrefixes matched
                )
                key
                (children trie)
          }

insert :: (Ord k) => [k] -> a -> TrieMap k a -> TrieMap k a
insert prefixes value = alter (const (Just value)) prefixes

insert' :: forall k a. (Ord k) => [k] -> TrieMap k a -> TrieMap k a -> TrieMap k a
insert' prefixes' value = go prefixes'
  where
    go :: [k] -> TrieMap k a -> TrieMap k a
    go prefixes trie = case prefixes of
      [] -> value `union` trie
      key : restPrefixes ->
        trie
          { children =
              Map.alter
                ( Just . \case
                    Nothing -> value
                    Just matched -> go restPrefixes matched
                )
                key
                (children trie)
          }

-- alter' :: (Ord k) => (Maybe (TrieMap k a) -> Maybe (TrieMap k a)) -> [k] -> TrieMap k a -> TrieMap k a
-- alter' f = go
--   where
--     go prefixes trie = case prefixes of
--       [] -> trie { value =  f (value trie)}
--       key : restPrefixes ->
--         trie
--           {
--             children =
--               Map.alter

--           }

-- insertTrieMap :: (Ord k) => [k] -> TrieMap k a -> TrieMap k a -> TrieMap k a
-- insertTrieMap prefixes value

delete :: (Ord k) => [k] -> TrieMap k a -> TrieMap k a
delete = alter (const Nothing)

-- insert prefixes value trie = case prefixes of
--   [] -> trie {value = Just value}
--   key : rest ->
--     trie
--       { children =
--           Map.alter
--             -- (Just . maybe (insertFromEmpty rest value) (insert rest value))
--             -- (Just . insert rest value . fromMaybe empty)
--             ( Just . \case
--                 Nothing -> insertFromEmpty rest value
--                 Just matched -> insert rest value matched
--             )
--             key
--             (children trie)
--       }
--  in case Map.lookup key children' of
--       Nothing -> trie {children = Map.insert key (insertFromEmpty rest value) children'}
--       Just matched -> trie {children = Map.insert key (insert rest value matched) children'}

walk :: (Ord k) => [k] -> TrieMap k a -> Maybe (TrieMap k a)
walk prefixes trie = case prefixes of
  [] -> Just trie
  key : restPrefixes ->
    Map.lookup key (children trie)
      >>= walk restPrefixes

lookup :: (Ord k) => [k] -> TrieMap k a -> Maybe a
lookup prefixes trie = walk prefixes trie >>= value

-- lookup' :: (Ord k) => [k] -> TrieMap k a -> Maybe a
-- lookup' prefixes (Node value children) = fst $ foldr (\key (_, children) ->
--     case Map.lookup key children of
--       Nothing -> undefined
--       ) (value, children) prefixes

-- case Map.lookup key children of
--   Nothing -> Nothing
--   Just matched -> lookup rest matched

insertFromEmpty :: [k] -> a -> TrieMap k a
insertFromEmpty prefixes value =
  foldr
    (\key acc -> Node Nothing (Map.singleton key acc))
    (Node (Just value) Map.empty)
    prefixes

alterFromEmpty :: (Maybe a -> Maybe a) -> [k] -> TrieMap k a
alterFromEmpty f =
  foldr
    (\key acc -> Node Nothing (Map.singleton key acc))
    (Node (f Nothing) Map.empty)

unionWith :: (Ord k) => (a -> a -> a) -> TrieMap k a -> TrieMap k a -> TrieMap k a
unionWith = undefined

union :: (Ord k) => TrieMap k a -> TrieMap k a -> TrieMap k a
union left = foldr (\(prefixes, value) acc -> insert prefixes value acc) left . toList

traverse :: (Ord k, Applicative f) => (a -> f b) -> TrieMap k a -> f (TrieMap k b)
traverse f = go
  where
    go trie =
      ( case value trie of
          Nothing -> pure (Node Nothing)
          Just value' -> Node . Just <$> f value'
      )
        <*> P.traverse go (children trie)

-- depth-first search
traverseWithKey :: (Ord k, Applicative f) => ([k] -> a -> f b) -> TrieMap k a -> f (TrieMap k b)
traverseWithKey f trie =
  ( case value trie of
      Nothing -> pure (Node Nothing)
      Just value' -> Node . Just <$> f [] value'
  )
    <*> Map.traverseWithKey
      (\key child -> traverseWithKey (\prefixes -> f (key : prefixes)) child)
      (children trie)

toList :: TrieMap k a -> [([k], a)]
toList (Node value children) =
  let
    recursed = Map.toList $ toList <$> children
    flattened = concatMap (\(key, childrenList) -> first (key :) <$> childrenList) recursed
   in
    maybeToList (([],) <$> value) <> flattened

fromList :: (Ord k) => [([k], a)] -> TrieMap k a
fromList = foldr (\(key, value) acc -> insert key value acc) empty

fromToListTest :: IO ()
fromToListTest = do
  let trie = fromList @Int @Int [([1, 2, 3], 3), ([1, 2], 4), ([2, 3, 5], 6)]
  print trie
  print (toList trie)

traverseWithKeyTest :: IO ()
traverseWithKeyTest = do
  let trie = fromList @Int @Int [([], 1), ([1], 2), ([5], 6), ([1, 2, 3], 3), ([1, 2], 4), ([2, 3, 5], 6)]
  traversed1 <-
    traverseWithKey
      ( \prefixes value -> do
          print prefixes
          print value
          pure (prefixes, value)
      )
      trie
  traversed2 <-
    traverse
      ( \value -> do
          print value
          pure value
      )
      trie
  print traversed1
  print traversed2
  pure ()

unionTest :: IO ()
unionTest = do
  let bigTrie = fromList [([9, 8, 7], 2)]
  let trie = fromList @Int @Int [([], 1), ([1], 2), ([5], 6), ([1, 2, 3], 3), ([1, 2], 4), ([2, 3, 5], 6)]
  let bigTrie' = insert' [9, 8] trie bigTrie
  print (toList bigTrie')

insert'Test :: IO ()
insert'Test = do
  let bigTrie = fromList [([9, 8, 7], 2)]
  let trie = fromList @Int @Int [([], 1), ([1], 2), ([5], 6), ([1, 2, 3], 3), ([1, 2], 4), ([2, 3, 5], 6)]
  let bigTrie' = insert' [9, 8] trie bigTrie
  print (toList bigTrie')
