-- original from https://johnwhiles.com/posts/implementing-a-trie
{-# LANGUAGE ScopedTypeVariables #-}

module Trie.Trie where

import Data.Map.Strict qualified as M
import Data.Maybe

data Trie a
  = Node !(Maybe [a]) !(M.Map a (Trie a))
  deriving (Eq, Show)

empty :: Trie a
empty = Node Nothing M.empty

getValue :: (Applicative m, Monoid (m [a])) => Trie a -> m [a]
getValue (Node value _) = maybe mempty pure value

getChildren :: Trie a -> M.Map a (Trie a)
getChildren (Node _ children) = children

setChildren :: Trie a -> M.Map a (Trie a) -> Trie a
setChildren (Node value _) = Node value

insert :: forall a. (Ord a) => [a] -> Trie a -> Trie a
insert word trie = go word trie
  where
    go :: [a] -> Trie a -> Trie a
    go [] trie = Node (Just word) (getChildren trie)
    go (ch : rest) trie =
      let children = getChildren trie
       in case M.lookup ch children of
            Just matchingChildNode ->
              setChildren trie (M.insert ch (go rest matchingChildNode) children)
            Nothing ->
              setChildren trie $ M.insert ch (go rest empty) children

getWords :: Trie a -> [[a]]
getWords trie =
  getValue trie
    <> foldMap getWords (getChildren trie)

main :: IO ()
main = do
  print $
    getWords $
      insert "hey" $
        insert "hello" $
          insert "egg" empty
  print $
    getWords $
      insert [1, 2, 3] $
        insert [1, 4, 5] $
          insert [1, 50, 2] empty
