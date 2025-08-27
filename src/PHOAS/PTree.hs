module PHOAS.PTree where

import Data.Function

data PTree a v
  = Var v
  | Mu ([v] -> [PTree a v])
  | Empty
  | Fork a (PTree a v) (PTree a v)

newtype Tree a = Hide {reveal :: forall v. PTree a v}

t1 :: Tree Int
t1 = Hide (Mu (\(~(x : _)) -> [Fork 1 (Fork 2 (Var x) Empty) (Var x)]))

t2 :: Tree Int
t2 = Hide (Mu (\(~(x : y : _)) -> [Fork 1 (Var y) (Var x), Fork 2 (Var x) (Var y)]))

foldTree :: forall a b. (a -> b -> b -> b) -> b -> b -> Tree a -> b
foldTree combine initial empty tree = trans (reveal tree)
  where
    trans :: PTree a b -> b
    trans tree = case tree of
      Var v -> v
      Mu g -> head (map trans (g (repeat initial)))
      Empty -> empty
      Fork element left right -> combine element (trans left) (trans right)

cfoldTree :: forall a b. (a -> b -> b -> b) -> b -> Tree a -> b
cfoldTree combine empty tree = trans (reveal tree)
  where
    trans :: PTree a b -> b
    trans tree = case tree of
      Var v -> v
      Mu g -> head (fix (map trans . g))
      Empty -> empty
      Fork element left right -> combine element (trans left) (trans right)

tmap :: forall a b. (a -> b) -> Tree a -> Tree b
tmap f tree = Hide (pmap (reveal tree))
  where
    pmap :: forall v. PTree a v -> PTree b v
    pmap tree = case tree of
      Var v -> Var v
      Mu g -> Mu (fmap pmap . g)
      Empty -> Empty
      Fork element left right -> Fork (f element) (pmap left) (pmap right)

instance (Eq a) => Eq (Tree a) where
  (==) :: forall a. (Eq a) => Tree a -> Tree a -> Bool
  t1 == t2 = eq 0 (reveal t1) (reveal t2)
    where
      eq :: Int -> PTree a Int -> PTree a Int -> Bool
      eq _ (Var x) (Var y) = x == y
      eq n (Mu f) (Mu g) =
        let
          l1 = f (iterate succ n)
          l2 = g (iterate succ n)
         in
          and $ zipWith (eq (n + length l1)) l1 l2
      eq _ Empty Empty = True
      eq n (Fork x left right) (Fork x' left' right') =
        x == x' && eq n left left' && eq n right right'
      eq _ _ _ = False

pjoin :: forall a v. PTree a (PTree a v) -> PTree a v
pjoin tree = case tree of
  Var v -> v
  Mu f -> Mu (fmap pjoin . f . fmap Var)
  Empty -> Empty
  Fork element left right -> Fork element (pjoin left) (pjoin right)

unrollTree :: forall a. Tree a -> Tree a
unrollTree tree = Hide (pjoin (unroll (reveal tree)))
  where
    unroll :: forall v. PTree a (PTree a v) -> PTree a (PTree a v)
    unroll tree = case tree of
      -- Mu g -> Mu (fmap unroll . g . fmap Var)
      Mu g -> head (g (repeat (pjoin (Mu g))))
      Empty -> Empty
      Fork element left right -> Fork element (unroll left) (unroll right)
      Var _ -> error ""
