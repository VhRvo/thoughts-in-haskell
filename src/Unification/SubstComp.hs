module Unification.SubstComp where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)

type Symbol = Text

data Term = Variable Symbol | Predicate Symbol [Term]

type Substitution = Map Symbol Term

subst :: Substitution -> Term -> Term
subst substitution = \case
  Variable symbol -> fromMaybe (Variable symbol) (Map.lookup symbol substitution)
  Predicate symbol terms -> Predicate symbol (fmap (subst substitution) terms)

-- Let's derive the composition of substitutions via equational reasoning,
-- satisfying `subst (comp s1 s2) term = subst s1 (subst s2 term)`.

{-
lemma 1:
    fmap f (lookup key map)
~=  case lookup key map of
        Nothing -> Nothing
        Just value -> Just (f value)
~=  lookup key (fmap f map)

lemma 2:
    fromMaybe (lookup key map1) (lookup key map2)
~=  case lookup key map2 of
        Nothing -> lookup key map1
        Just value -> value
~=  lookup key (map2 `union` (map1 `diff` map2))
~=  lookup key (map2 `union` map1) -- union is left-biased
-}

{-
    subst (comp s1 s2) term
=>  subst s1 (subst s2 term)
    case term of
        Variable symbol ->
            subst s1 (subst s2 (Variable symbol))
        =>  subst s1 (fromMaybe (Variable symbol) (lookup symbol s2))
        =>  fromMaybe (subst s1 (Variable symbol)) (fmap (subst s1) (lookup symbol s2))
        =>  fromMaybe (lookup symbol s1) (fmap (subst s1) (lookup symbol s2))
        =>  fromMaybe (lookup symbol s1) (lookup symbol (fmap (subst s1) s2))
        =>  lookup symbol (fmap (subst s1) s2 `union` s1)
        Predicate symbol terms ->
            subst s1 (Predicate symbol (fmap (subst s2) terms))
        =>  Predicate symbol (fmap (subst s1) (fmap (subst s2) terms))
        =>  Predicate symbol (fmap (subst s1 . subst s2) terms)
        =>  Predicate symbol (fmap (\term -> subst s1 (subst s2 term)) terms)
        =>  Predicate symbol (fmap (\term -> subst (comp s1 s2) term) terms)
        =>  Predicate symbol (fmap (subst (comp s1 s2)) terms)
-}

-- So, a leap of faith, we define `comp s1 s2 = fmap (subst s1) s2 `union` s1`!
