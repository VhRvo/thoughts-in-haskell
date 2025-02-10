module Zipper.General where

import Data.Text (Text)

data Tree a
  = Item a
  | Section [Tree a]
  deriving (Eq, Show)

-- A Path is like a zipper, allowing on to rip the tree structure down to a certain location.
data Path a
  = Top
  | Node [Tree a] (Path a) [Tree a]
  deriving (Eq, Show)

-- A Location in the tree addresses a subtree, together with its path.
data Location a = Location (Tree a) (Path a)
  deriving (Eq, Show)

tree1 :: Tree [Char]
tree1 =
  Section
    [ Section [Item "a", Item "*", Item "b"],
      Item "+",
      Section [Item "c", Item "*", Item "d"]
    ]

location1 :: Location [Char]
location1 =
  Location
    (Item "*")
    ( Node
        [Item "c"]
        ( Node
            [ Item "+",
              Section [Item "a", Item "*", Item "b"]
            ]
            Top
            []
        )
        [Item "d"]
    )

from :: Location a -> Tree a
from (Location focus' path') = from' focus' path'
-- from (Location (from' -> result)) = result
  where
    from' :: Tree a -> Path a -> Tree a
    from' focus = \case
      Top -> focus
      Node left path right ->
        from' (Section $ reverse left <> (focus : right)) path

goLeft :: Location a -> Either Text (Location a)
goLeft (Location focus path) = case path of
  Top -> Left "left of top"
  Node (l : left) up right ->
    pure $ Location l (Node left up (focus : right))
  _ -> Left "left of first"

goRight :: Location a -> Either Text (Location a)
goRight (Location focus path) = case path of
    Top -> Left "right of top"
    Node left up (r:right) ->
        pure $ Location r (Node (focus : left) up right)
    _ -> Left "right of last"

goUp :: Location a -> Either Text (Location a)
goUp (Location focus path) = case path of
    Top -> Left "up of top"
    Node left up right -> pure $ Location (Section $ reverse left <> (focus : right)) up

goDown :: Location a -> Either Text (Location a)
goDown (Location focus path) = case focus of
    Item _ -> Left "down of item"
    Section (first : rest) -> pure $ Location first (Node [] path rest)
    _ -> Left "down of empty"

nth :: Location a -> Int -> Either Text (Location a)
nth location = \case
    1 -> goDown location
    n | n > 0 -> goRight location >>= (`nth`(n - 1))
      | otherwise -> Left "nth expects a positive integer"

-- >>> from location1

-- Mutate the structure at the current location as a local operation
change :: Location a -> Tree a -> Location a
change (Location _ path) tree = Location tree path

-- Insertion to the left or to the right is natural and cheap
insertRight :: Location a -> Tree a -> Either Text (Location a)
insertRight (Location focus path) tree = case path of
    Top -> Left "insert of top"
    Node left up right -> pure $ Location focus (Node left up (tree : right))

insertLeft :: Location a -> Tree a -> Either Text (Location a)
insertLeft (Location focus path) tree = case path of
    Top -> Left "insert of top"
    Node left up right -> pure $ Location focus (Node (tree : left) up right)

insertDown :: Location a -> Tree a -> Either Text (Location a)
insertDown (Location focus path) tree = case focus of
    Item _ -> Left "down of item"
    Section children -> pure $ Location tree (Node [] path children)

-- We may choose to move right, if possible, otherwise left,
-- and up in case of an empty list.
delete :: Location a -> Either Text (Location a)
delete (Location _ path) = case path of
    Top -> Left "delete of top"
    Node left up (r:right) -> pure $ Location r (Node left up right)
    Node (l:left) up [] -> pure $ Location l (Node left up [])
    Node [] up [] -> pure $ Location (Section []) up

-- data MemoTree a
--     = Item a
