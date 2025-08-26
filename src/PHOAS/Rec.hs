{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module PHOAS.Rec where

data Rec f a
  = Var a
  | Mu ([a] -> [f (Rec f a)])
  | In (f (Rec f a))

newtype Graph f = Hide { reveal :: forall a. Rec f a }

data StreamF a r = Cons a r
  deriving (Functor, Foldable, Traversable)

type Stream a = Graph (StreamF a)

stream :: Stream Int
stream = Hide (Mu (\(~(x:_)) -> [Cons 1 (In (Cons 2 (Var x)))]))

data TreeF a r = Empty | Fork a r r
  deriving (Functor, Foldable, Traversable)

type Tree a = Graph (TreeF a)

tree :: Tree Int
tree = Hide (Mu (\(~(t1 : t2 : t3 : _)) -> [
    Fork 1 (In (Fork 4 (Var t2) (In Empty))) (Var t3),
    Fork 2 (Var t1) (Var t3),
    Fork 3 (Var t2) (Var t1)
    ]))
