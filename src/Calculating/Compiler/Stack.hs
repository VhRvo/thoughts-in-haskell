{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Stack where

data Exp
  = Val Int
  | Add Exp Exp

eval :: Exp -> Int
eval =
  \case
    Val n -> n
    Add x y -> eval x + eval y

type Stack = [Int]

eval' :: Exp -> Stack -> Stack
eval' exp stack =
  case exp of
    Val n -> push n stack
    Add x y -> add (eval' y (eval' x stack))

push :: a -> [a] -> [a]
push n s = n : s

add :: Num a => [a] -> [a]
add (n : m : s) = m + n : s
add _ = error "compiler bug!"

type Cont = Stack -> Stack

eval'' :: Exp -> Stack -> Cont -> Stack
eval'' exp stack k =
  case exp of
    Val n -> k (push n stack)
    Add x y -> eval'' x stack (\sx -> eval'' y sx (\sy -> k (add sy)))

data CONT
  = Id
  | OAdd Exp CONT
  | IAdd CONT

-- applyCONT :: CONT -> Stack -> Stack
-- applyCONT cont =
--   case cont of
--     Id -> id
--     OAdd exp rest -> \sx -> eval''

eval1'' :: Exp -> Cont -> Cont
eval1'' exp k stack =
  case exp of
    Val n -> k (push n stack)
    Add x y -> eval1'' x (\sx -> eval1'' y (\sy -> k (add sy)) sx) stack

eval2'' :: Exp -> Cont -> Cont
eval2'' exp k =
  case exp of
    Val n -> k . push n
    Add x y -> eval2'' x (eval2'' y (k . add))

