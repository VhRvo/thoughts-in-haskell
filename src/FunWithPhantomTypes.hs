{-# LANGUAGE NoImplicitPrelude #-}

module FunWithPhantomTypes where

import Prelude hiding (compare)

import Data.Bifunctor

import Data.Char (ord)
import qualified Data.Ord as O

data Term t
    = (t ~ Int) => Zero
    | (t ~ Int) => Succ (Term Int)
    | (t ~ Int) => Pred (Term Int)
    | (t ~ Bool) => IsZero (Term Int)
    | forall a. (t ~ a) => If (Term Bool) (Term a) (Term a)

eval :: forall t. Term t -> t
eval term = case term of
    Zero  -> 0
    Pred  expr -> eval @Int expr - 1
    Succ  expr -> eval @Int expr + 1
    IsZero expr -> eval @Int expr == 0
    If @a cond trueExpr falseExpr ->
        if eval @Bool cond then eval @a trueExpr else eval @a falseExpr

type Name = String
type Age = Int
data Person = Person Name Age
    deriving (Eq, Ord, Show)

data Type t
    = (t ~ Int) => RInt
    | (t ~ Char) => RChar
    | forall a. (t ~ [a]) => RList (Type a)
    | forall a b. (t ~ (a, b)) => RPair (Type a) (Type b)
    | (t ~ Person) => RPerson
    | (t ~ Dynamic) => RDynamic

data Dynamic = forall t. Dynamic (Type t) t

tEqual :: forall t u. Type t -> Type u -> Maybe (t -> u)
tEqual lhs rhs = case (lhs, rhs) of
    (RInt, RInt) -> pure id
    (RChar, RChar) -> pure id
    (RList rep1, RList rep2) -> fmap <$> tEqual rep1 rep2
    (RPair rep1 rep2, RPair rep1' rep2') -> bimap <$> tEqual rep1 rep1' <*> tEqual rep2 rep2'
    (RPerson, RPerson) -> pure id
    _ -> fail "cannot tEqual"

cast :: forall t. Dynamic -> Type t -> Maybe t
cast (Dynamic @d rep term) rep' = fmap (\f -> f term) (tEqual @d @t rep rep')

rString :: Type String
rString = RList RChar

data Bit = Z | O
    deriving (Eq, Ord, Show)

compress :: forall t. Type t -> t -> [Bit]
compress rep term = case rep of
    RInt -> compressInt term
    RChar -> compressChar term
    RList rep' -> case term of
        [] -> Z : []
        t:ts -> O : compress rep' t <> compress rep ts
    RPair rep1 rep2 -> case term of
        (x, y) -> compress rep1 x <> compress rep2 y
    RPerson -> compressPerson term
    RDynamic -> case term of
        Dynamic @d rep' term' -> compress @d rep' term'
  where
    compressInt :: Int -> [Bit]
    compressInt n
        | n == 0 = [Z]
        | n `mod` 2 == 0 = O : compressInt n'
        | otherwise = Z : compressInt n'
        where
            n' = n `div` 2

    compressChar  :: Char -> [Bit]
    compressChar ch = compressInt (ord ch)

    compressPerson :: Person -> [Bit]
    compressPerson = undefined


eq :: forall t. Type t -> t -> t -> Bool
eq rep lhs rhs = case rep of
    RInt -> lhs == rhs
    RChar -> lhs == rhs
    RList rep' -> case (lhs, rhs) of
        ([], []) -> True
        (lHead: lTail, rHead: rTail) -> eq rep' lHead rHead && eq rep lTail rTail
        _ -> False
    RPair rep1 rep2 -> case (lhs, rhs) of
        ((lFst, lSnd), (rFst, rSnd)) -> eq rep1 lFst rFst && eq rep2 lSnd rSnd
    RPerson -> lhs == rhs
    RDynamic -> case lhs of
        Dynamic @d rep1 term1 -> case cast @d rhs rep1 of
            Just term2 -> eq rep1 term1 term2
            Nothing -> False

compare :: forall t. Type t -> t -> t -> Ordering
compare rep lhs rhs =  case rep of
    RInt -> O.compare lhs rhs
    RChar -> O.compare lhs rhs
    RList rep' -> case (lhs, rhs) of
        ([], []) -> mempty
        ([], _:_) -> O.LT
        (_:_, [])  -> O.GT
        (lHead: lTail, rHead: rTail) -> compare rep' lHead rHead <> compare rep lTail rTail
    RPair rep1 rep2 -> case (lhs, rhs) of
        ((lFst, lSnd), (rFst, rSnd)) -> compare rep1 lFst rFst <> compare rep2 lSnd rSnd
    RPerson -> O.compare lhs rhs
    RDynamic -> case lhs of
        Dynamic @d rep1 term1 -> case cast @d rhs rep1 of
            Just term2 -> compare rep1 term1 term2
            Nothing -> LT

type Traversal = forall t. Type t -> t -> t

tick :: Name -> Traversal
tick target RPerson person@(Person name age)
  | target == name = Person name (age + 1)
  | otherwise = person

copy :: Traversal
copy _ = id

compose :: Traversal -> Traversal -> Traversal
compose f g rep = f rep . g rep

imap :: forall t. (forall t. Type t -> t -> t) -> Type t -> t -> t
imap f rep term = case rep of
    RInt -> term
    RChar -> term
    RList rep' -> case term of
        [] -> []
        r:rs -> f rep' r : f (RList rep') rs
    RPair rep1 rep2 -> case term of
        (fst, snd) -> (f rep1 fst, f rep2 snd)
    RPerson -> case term of
        Person name age -> Person (f rString name) (f RInt age)
    RDynamic -> case term of
        Dynamic @d rep' term' -> Dynamic @d rep' (f rep' term')

