module Trie.TrieMap where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prelude hiding (lookup)

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
      key : rest ->
        trie
          { children =
              Map.alter
                ( Just . \case
                    Nothing -> alterFromEmpty f prefixes
                    Just matched -> go rest matched
                )
                key
                (children trie)
          }

insert :: (Ord k) => [k] -> a -> TrieMap k a -> TrieMap k a
insert prefixes value = alter (const (Just value)) prefixes

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
  key : rest ->
    Map.lookup key (children trie)
      >>= walk rest

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
