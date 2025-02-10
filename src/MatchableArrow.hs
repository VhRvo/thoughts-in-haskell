module MatchableArrow where

import Data.Kind (Type)

-- Because (->) is just a data constructor at type level,
-- i.e. (->) is a type constructor
type family Dispatch arrow :: Type where
    Dispatch (Int -> Bool) = Bool
    Dispatch (Bool -> Bool) = Bool

type family Dispatch1 arrow :: Type where
    Dispatch1 Maybe = Bool
    Dispatch1 (Either Int) = Bool
