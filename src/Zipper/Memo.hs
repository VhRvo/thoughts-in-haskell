module Zipper.Memo where

import Data.Text (Text)

data MemoTree a
  = Item a
  | Sibling [MemoTree a] (MemoTree a) [MemoTree a]
  deriving (Eq, Ord, Show)

data MemoPath a
  = Top
  | Node [MemoTree a] (MemoPath a) [MemoTree a]
  deriving (Eq, Ord, Show)

data Location a = Location (MemoTree a) (MemoPath a)
  deriving (Eq, Ord, Show)

goUpMemo :: Location a -> Either Text (Location a)
goUpMemo (Location focus path) = case path of
  Top -> Left "up of top"
  Node left up right ->
    pure $ Location (Sibling left focus right) up

goDownMemo :: Location a -> Either Text (Location a)
goDownMemo (Location focus path) = case focus of
  Item _ -> Left "down of item"
  Sibling left tree right -> pure $ Location tree (Node left path right)
