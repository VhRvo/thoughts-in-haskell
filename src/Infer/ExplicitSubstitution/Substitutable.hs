module Infer.ExplicitSubstitution.Substitutable where

class Substitutable a where
    freeVars :: a -> Set Identifier
    apply :: Substitution -> a -> a

