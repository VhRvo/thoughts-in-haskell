module Trie.TrieMap where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Prelude hiding (lookup)

data TrieMap k v
  = -- = Node { file :: Map Text a, directory :: Map Text (TrieMap a) }
  Node {value :: Maybe v, children :: Map k (TrieMap k v)}
  deriving (Show)

empty :: TrieMap k v
empty = Node Nothing Map.empty

insert :: (Ord k) => [k] -> a -> TrieMap k a -> TrieMap k a
insert prefixes value trie = case prefixes of
  [] -> trie {value = Just value}
  key : rest ->
    trie
      { children =
          Map.alter
            -- (Just . maybe (insertFromEmpty rest value) (insert rest value))
            -- (Just . insert rest value . fromMaybe empty)
            ( Just . \case
                Nothing -> insertFromEmpty rest value
                Just matched -> insert rest value matched
            )
            key
            (children trie)
      }

--  in case Map.lookup key children' of
--       Nothing -> trie {children = Map.insert key (insertFromEmpty rest value) children'}
--       Just matched -> trie {children = Map.insert key (insert rest value matched) children'}

lookup :: (Ord k) => [k] -> TrieMap k a -> Maybe a
lookup prefixes (Node value children) = case prefixes of
  [] -> value
  key : rest ->
    Map.lookup key children
      >>= lookup rest

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
