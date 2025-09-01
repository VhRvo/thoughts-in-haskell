module Bound.Naive where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.List (elemIndex)

type Name = Text

data Expr
  = Bound Int
  | Free Name
  | App Expr Expr
  | Lam Name Expr
  deriving (Eq, Show, Read)

debruijn :: Map Name Int -> Expr -> Expr
debruijn env = \case
  Bound index ->
    Bound index
  Free name ->
    maybe (Free name) Bound (Map.lookup name env)
  App fun arg ->
    App (debruijn env fun) (debruijn env arg)
  Lam name body ->
    debruijn (Map.insert name 0 (fmap (+ 1) env)) body

-- hesitating
-- Is the following code consider cases where two names are the same?
-- No, if you insert the same name more than twice, the value of the name in map is always the same.
debruijn' :: Map Name Int -> Expr -> Expr
debruijn' env = \case
  Bound index ->
    Bound index
  Free name ->
    maybe (Free name) (Bound . ((length env - 1) -)) (Map.lookup name env)
  App fun arg ->
    App (debruijn' env fun) (debruijn' env arg)
  Lam name body ->
    debruijn' (Map.insert name (length env) env) body

-- the two algorithms can be unified to one algorithm
-- where the difference is the map.

-- Map0 is the trivial implementation of list,
-- it's very slow, because when the lookup operation need to
-- traverse the whole list to get its position/index.

newtype Map0 = Map0 [Name]

empty0 :: Map0
empty0 = Map0 []

insert0 :: Name -> Map0 -> Map0
insert0 name (Map0 map) = Map0 (name : map)

lookup0 :: Name -> Map0 -> Maybe Int
lookup0 name (Map0 map) = elemIndex name map

-- Map1 don't depend on the size of the map,
-- it encode the information of size into the values in the map.
newtype Map1 = Map1 (Map Name Int)

empty1 :: Map1
empty1 = Map1 Map.empty

-- Map1 don't have same name problems,
-- it always assign 0 to every new element,
-- it donesn't depend on the size of the map.
insert1 :: Name -> Map1 -> Map1
insert1 name (Map1 map) =
  Map1 (Map.insert name 0 (fmap (+ 1) map))

lookup1 :: Name -> Map1 -> Maybe Int
lookup1 name (Map1 map) =
  Map.lookup name map

-- Map1 has performance problem,
-- when inserting element into the map,
-- it traverses the whole map.
-- I want to lazify the traversal.

-- We should remember the number of elements after
-- one element inserted.
-- The following implement has same-name problem,
-- when many same names to be inserted, the length of map
-- is not equal the number of elements inserted,
-- we need to remember the element manually.
newtype Map2 = Map2 (Map Name Int)

empty2 :: Map2
empty2 = Map2 Map.empty

insert2 :: Name -> Map2 -> Map2
insert2 name (Map2 map) =
  Map2(Map.insert name (length map) map)

lookup2 :: Name -> Map2 -> Maybe Int
lookup2 name (Map2 map) =
  ((length map - 1) -) <$> Map.lookup name map

data Map3 = Map3 Int (Map Name Int)

empty3 :: Map3
empty3 = Map3 0 Map.empty

insert3 :: Name -> Map3 -> Map3
insert3 name (Map3 size map) =
  Map3 (size + 1) (Map.insert name size map)

lookup3 :: Name -> Map3 -> Maybe Int
lookup3 name (Map3 size map) =
  ((size - 1) -) <$> Map.lookup name map

-- we can not inline the substract one from size by initializing the map with -1 size,
-- because we also use the size in insert operation.

-- we need to prove the equalivalence between Map0 and Map3.
-- I think we only need to prove the following theorem.

-- Map0=Map1 : forall name map0 map3. lookup0 name map0 = lookup3 name map3

-- empty-same : forall name. Map0=Map1 name empty0 empty3
-- and
-- same-after-inserting : forall map0 map3 name value. Map0=Map1 name map0 map3 -> Map0=Map1 name (insert0 name map0) (insert3 name map3)

-- I don't know how to prove theorem about Map.
-- Maybe I should learn denotational semantics.


