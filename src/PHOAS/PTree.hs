module PHOAS.PTree where

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
foldTree f k1 k2 tree = trans (reveal tree)
  where
    trans :: PTree a b -> b
    trans tree = case tree of
      Var v -> v
      Mu f -> undefined
      Empty -> k2
      Fork element left right -> f element (trans left) (trans right)
