module Trie.TrieMap where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

data TrieMap k v
  -- = Node { file :: Map Text a, directory :: Map Text (TrieMap a) }
  = Node { value :: Maybe v, children :: Map k (TrieMap k v) }
  deriving (Show)

empty :: TrieMap k v
empty = Node Nothing Map.empty

insert :: (Ord k) => [k] -> a -> TrieMap k a -> TrieMap k a
insert prefixes value trie = case prefixes of
  [] -> trie { value = Just value }
  key : rest ->
    let children' = children trie
     in case Map.lookup key children' of
          Nothing -> trie { children = Map.insert key (insert rest value empty) children' }
          Just matched -> trie { children = Map.insert key (insert rest value matched) children' }

insertFromEmpty :: [k] -> a -> TrieMap k a
insertFromEmpty prefixes value = foldr (\key acc -> Node Nothing (Map.singleton key acc)) (Node (Just value) Map.empty) prefixes
