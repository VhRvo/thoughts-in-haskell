{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module PHOAS.Rec where

import Data.Bifunctor
import Data.Function

data Rec f a
  = Var a
  | Mu ([a] -> [f (Rec f a)])
  | In (f (Rec f a))

newtype Graph f = Hide {reveal :: forall a. Rec f a}

data StreamF a r = Cons a r
  deriving (Functor, Foldable, Traversable)

type Stream a = Graph (StreamF a)

stream :: Stream Int
stream = Hide (Mu (\(~(x : _)) -> [Cons 1 (In (Cons 2 (Var x)))]))

data TreeF a r = Empty | Fork a r r
  deriving (Functor, Foldable, Traversable)

type Tree a = Graph (TreeF a)

tree :: Tree Int
tree =
  Hide
    ( Mu
        ( \(~(t1 : t2 : t3 : _)) ->
            [ Fork 1 (In (Fork 4 (Var t2) (In Empty))) (Var t3),
              Fork 2 (Var t1) (Var t3),
              Fork 3 (Var t2) (Var t1)
            ]
        )
    )

myGfold :: forall f r. (Functor f) => r -> (f r -> r) -> Graph f -> r
myGfold initial algebra graph = gfold' (reveal graph)
  where
    gfold' :: Rec f r -> r
    gfold' (Var a) = a
    gfold' (Mu g) = head $ fmap (algebra . fmap gfold') (g (repeat initial))
    gfold' (In fa) = algebra (fmap gfold' fa)

gfold :: forall f t r. (Functor f) => (t -> r) -> (([t] -> [r]) -> r) -> (f r -> r) -> Graph f -> r
gfold initial recursor algebra graph = trans (reveal graph)
  where
    trans :: Rec f t -> r
    trans (Var a) = initial a
    trans (Mu g) = recursor (fmap (algebra . fmap trans) . g)
    trans (In fa) = algebra (fmap trans fa)

fold :: forall f r. (Functor f) => (f r -> r) -> r -> Graph f -> r
fold algebra initial = gfold id (\g -> head (g (repeat initial))) algebra

cfold :: forall f r. (Functor f) => (f r -> r) -> Graph f -> r
cfold = gfold id (\g -> head (fix g))

sfold :: (Eq t, Functor f) => (f t -> t) -> t -> Graph f -> t
sfold algebra initial = gfold id (head . fixVal (repeat initial)) algebra

fixVal :: forall a. (Eq a) => a -> (a -> a) -> a
fixVal v f
  | v == v' = v
  | otherwise = fixVal v' f
  where
    v' = f v

type f ~> g = forall a. f a -> g a

transform :: forall f g. (Functor f, Functor g) => (f ~> g) -> Graph f -> Graph g
transform alpha graph = Hide (hmap (reveal graph))
  where
    hmap :: forall a. Rec f a -> Rec g a
    hmap (Var x) = Var x
    hmap (Mu g) = Mu (\x -> fmap (fmap hmap . alpha) (g x))
    hmap (In fa) = In (alpha (fmap hmap fa))

gmap ::
  (Bifunctor f, Functor (f a), Functor (f b)) =>
  (a -> b) ->
  Graph (f a) ->
  Graph (f b)
gmap f = transform (first f)

pjoin :: forall f a. (Functor f) => Rec f (Rec f a) -> Rec f a
pjoin (Var x) = x
-- pjoin (Mu g) = Mu (\x -> fmap (fmap pjoin) (g (fmap Var x)))
pjoin (Mu g) = Mu (fmap (fmap pjoin) . g . fmap Var)
pjoin (In fa) = In (fmap pjoin fa)

unrollGraph :: forall f. (Functor f) => Graph f -> Graph f
unrollGraph graph = Hide (pjoin (unroll (reveal graph)))

unroll :: forall f a. (Functor f) => Rec f (Rec f a) -> Rec f (Rec f a)
unroll (Mu g) =
  -- let x = pjoin (Mu g)
  --     y = g (repeat x)
  --     -- z = fmap (fmap pjoin) y
  --     w = In (head y)
  --     -- y = head (fmap unroll (g (repeat x)))
  -- in w
  In (head . g . repeat . pjoin . Mu $ g)
unroll (In fa) = In (fmap unroll fa)
unroll (Var _) = error ""

data VGraphF a = VNode String [a]
  deriving (Show, Functor, Foldable, Traversable)

type VGraph = Graph VGraphF

btree2vgraph :: forall a. (Show a) => Tree a -> VGraph
btree2vgraph = transform trans
  where
    trans Empty = VNode "" []
    trans (Fork x l r) = VNode (show x) [l, r]

geq :: forall f. (EqF f) => Graph f -> Graph f -> Bool
geq g1 g2 = eqRec 0 (reveal g1) (reveal g2)

eqRec :: forall f. (EqF f) => Int -> Rec f Int -> Rec f Int -> Bool
eqRec _ (Var x) (Var y) = x == y
eqRec n (Mu g1) (Mu g2) =
  let
    g1' = g1 (iterate succ n)
    g2' = g2 (iterate succ n)
   in
    and $ zipWith (eqF (eqRec (n + length g1'))) g1' g2'
eqRec n (In fx) (In fy) = eqF (eqRec n) fx fy
eqRec _ _ _ = False

class (Functor f) => EqF f where
  eqF :: (r -> r -> Bool) -> f r -> f r -> Bool

instance (Eq a) => EqF (StreamF a) where
  eqF :: forall r a. (Eq a) => (r -> r -> Bool) -> StreamF a r -> StreamF a r -> Bool
  eqF eq (Cons x xs) (Cons y ys) = x == y && eq xs ys

showGraph :: forall f. (ShowF f) => Graph f -> String
showGraph graph = showRec (iterate succ 'a') (reveal graph)

showRec :: forall f. (ShowF f) => [Char] -> Rec f Char -> String
showRec _ (Var c) = [c]
showRec chars (Mu g) =
  let
    result = g chars
    (used, rest) = splitAt (length result) chars
   in
    "Mu (\n"
      <> concat
        [ "  " <> [a] <> " => " <> v <> "\n"
          | (a, v) <- zip used (fmap (showF (showRec rest)) result)
        ]
      <> ")\n"
showRec chars (In fa) = showF (showRec chars) fa

class (Functor f) => ShowF f where
  showF :: (r -> String) -> f r -> String

instance (Show a) => ShowF (TreeF a) where
  showF :: forall r a. (Show a) => (r -> String) -> TreeF a r -> String
  showF _ Empty = "Empty"
  showF show' (Fork x l r) = "Fork" <> show x <> "(" <> show' l <> ") <" <> show' r <> ")"

