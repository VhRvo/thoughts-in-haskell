> module Unification.SubstComp where

> import Data.Map (Map)
> import Data.Map qualified as Map
> import Data.Maybe
> import Data.Text (Text)

> type Symbol = Text

> data Term = Variable Symbol | Predicate Symbol [Term]

> type Substitution = Map Symbol Term

> subst :: Substitution -> Term -> Term
> subst substitution = \case
>   Variable symbol -> fromMaybe (Variable symbol) (Map.lookup symbol substitution)
>   Predicate symbol terms -> Predicate symbol (fmap (subst substitution) terms)

Let's derive the composition of substitutions via equational reasoning, satisfying `subst (comp s1 s2) term = subst s1 (subst s2 term)`.

lemma 1:
    fmap f (lookup key map)
<=> case lookup key map of
        Nothing -> Nothing
        Just value -> Just (f value)
<=> lookup key (fmap f map)

lemma 2:
    fromMaybe (fromMaybe (Variable key) (lookup key map1)) (lookup key map2)
<=> case lookup key map2 of
        Nothing -> fromMaybe (Variable key) (lookup key map1)
        Just value -> value
<=> case lookup key map2 of
        Nothing -> case lookup key map1 of
            Nothing -> Variable key
            Just value2 -> value2
        Just value -> value
<=> fromMaybe (Variable key) (lookup key (map2 `union` (map1 `diff` map2)))
<=> fromMaybe (Variable key) (lookup key (map2 `union` map1))  -- if union is left-biased

    subst (comp s1 s2) term
<=> subst s1 (subst s2 term)
    case term of
        Variable symbol ->
            subst s1 (subst s2 (Variable symbol))
        <=> subst s1 (fromMaybe (Variable symbol) (lookup symbol s2))
        <=> case lookup symbol s2 of
                Nothing -> subst s1 (Variable symbol)
                Just value -> subst s1 value
        <=> fromMaybe (subst s1 (Variable symbol)) (fmap (subst s1) (lookup symbol s2))
        <=> fromMaybe (fromMaybe (Variable symbol) (lookup symbol s1)) (fmap (subst s1) (lookup symbol s2))
            {- lemma 1: fmap f (lookup key map)
                    <=> lookup key (fmap f map) -}
        <=> fromMaybe (fromMaybe (Variable symbol) (lookup symbol s1)) (lookup symbol (fmap (subst s1) s2))
            {- lemma 2: fromMaybe (fromMaybe (Variable key) (lookup key map1)) (lookup key map2)
                    <=> fromMaybe (Variable key) (lookup key (map2 `union` map1)) -}
        <=> fromMaybe (Variable symbol) (lookup symbol (fmap (subst s1) s2 `union` s1))
        <=> subst (Variable symbol) (fmap (subst s1) s2 `union` s1)
        <=> subst (Variable symbol) (comp s1 s2)
        Predicate symbol terms ->
            Predicate symbol (fmap (subst (comp s1 s2)) terms)
         -- Ridiculous transformation:
            subst s1 (Predicate symbol (fmap (subst s2) terms))
        <=> Predicate symbol (fmap (subst s1) (fmap (subst s2) terms))
        <=> Predicate symbol (fmap (subst s1 . subst s2) terms)
        <=> Predicate symbol (fmap (\term -> subst s1 (subst s2 term)) terms)
        <=> Predicate symbol (fmap (\term -> subst (comp s1 s2) term) terms)
        <=> Predicate symbol (fmap (subst (comp s1 s2)) terms)

So, a leap of faith, we define `comp s1 s2 = fmap (subst s1) s2 `union` s1`!

> comp :: Substitution -> Substitution -> Substitution
> comp s1 s2 = fmap (subst s1) s2 `Map.union` s1
