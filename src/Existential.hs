module Existential where

data Showable = forall x. Show x => Showable x

