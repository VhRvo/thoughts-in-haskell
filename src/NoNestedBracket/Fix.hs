module NoNestedBracket.Fix where

import Data.Functor.Const
import Data.Map
import Data.Monoid
import Data.Void

-- newtype Const a b = Const { getConst :: a }
-- type C = Const Int
sum :: Map k Int -> Int
-- sum map =  getSum (getConst(traverse (\a -> Const (Sum a)) map))
sum map = getSum (getConst (traverse (Const . Sum) map))

-- Applicative is Const (Sum Int)
-- Int -> Const (Sum Int) Int
-- traverse :: (a -> f b) -> t a -> f (t b)
-- traverse :: (Int -> Const (Sum Int) (Sum Int)) -> Map k Int -> Const (Sum Int) (Map k (Sum Int))

{-
Type = Int | Type -> Type | Bracket Type
Bracket (Bracket x): wrong
Bracket (Bracket x -> Int): wrong
-}

data TypeF other r
  = Int
  | Arrow r r
  | Other (other Type0)

newtype BracketF r
  = Bracket r

newtype Type0 = Type0 (TypeF (Const Void) Type0)

newtype Type1 = Type1 (TypeF BracketF Type1)

x :: Type1
x = Type1 (Other (Bracket (Type0 (Arrow undefined undefined))))

y :: Type0 -> Type0 -> Type1
y a b = Type1 (Other (Bracket (Type0 (Arrow a b))))
