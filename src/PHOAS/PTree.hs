module PHOAS.PTree where

data PTree a v
  = Var v
  | Mu ([v] -> [PTree a v])
  | Empty
  | Fork a (PTree a v) (PTree a v)

newtype Tree a = Hide { reveal :: forall v. PTree a v }

t1 :: Tree Int
t1 = Hide (Mu (\(~(x:_)) -> [Fork 1 (Fork 2 (Var x) Empty) (Var x)]))

t2 :: Tree Int
t2 = Hide (Mu (\(~(x:y:_)) -> undefined))