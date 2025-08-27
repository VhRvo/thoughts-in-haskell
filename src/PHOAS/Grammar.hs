{-# LANGUAGE DeriveTraversable #-}

module PHOAS.Grammar where

import PHOAS.Rec

data PatternF a
  = Term String
  | E
  | Seq a a
  | Alt a a
  deriving (Functor, Foldable, Traversable)

nullF :: PatternF Bool -> Bool
nullF pat = case pat of
  Term _ -> False
  E -> True
  Seq p1 p2 -> p1 && p2
  Alt p1 p2 -> p1 || p2

nullable :: Graph PatternF -> Bool
nullable = sfold nullF False
