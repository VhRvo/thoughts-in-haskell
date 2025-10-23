{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Stack1 where

data Exp
  = Val Int
  | Add Exp Exp

type Stack = [Int]

push :: a -> [a] -> [a]
push n s = n : s

add :: Num a => [a] -> [a]
add (n : m : s) = m + n : s
add _ = error "compiler bug!"

-- type Cont = Stack -> Stack
data Cont
  = Id
  | OAdd Exp Cont
  | IAdd Cont

applyCont :: Cont -> Stack -> Stack
applyCont cont =
  case cont of
    Id -> id
    OAdd y k -> \sx -> eval y sx (IAdd k)
    IAdd k -> \sy -> applyCont k (add sy)

eval :: Exp -> Stack -> Cont -> Stack
eval exp stack k =
  case exp of
    Val n -> applyCont k (push n stack)
    Add x y -> eval x stack (OAdd y k)
