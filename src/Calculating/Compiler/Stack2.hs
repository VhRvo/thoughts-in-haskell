{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Stack2 where

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

data Action'
  = OAdd' Exp
  | IAdd'

applyCont1 :: [Action'] -> Stack -> Stack
applyCont1 cont =
  case cont of
    [] -> id
    OAdd' y : k -> \sx -> eval1 y sx (IAdd' : k)
    IAdd' : k -> \sy -> applyCont1 k (add sy)

eval1 :: Exp -> Stack -> [Action'] -> Stack
eval1 exp stack k =
  case exp of
    Val n -> applyCont1 k (push n stack)
    Add x y -> eval1 x stack (OAdd' y : k)

applyCont2 :: [Action'] -> Stack -> Stack
applyCont2 cont =
  case cont of
    [] -> id
    OAdd' y : k -> \sx -> eval2 y (IAdd' : k) sx
    IAdd' : k -> \sy -> applyCont2 k (add sy)

eval2 :: Exp -> [Action'] -> Stack -> Stack
eval2 exp k stack =
  case exp of
    Val n -> applyCont2 k (push n stack)
    Add x y -> eval2 x (OAdd' y : k) stack

data Action''
  = Exp'' Exp
  | Add''

applyCont1' :: [Action''] -> Stack -> Stack
applyCont1' cont =
  case cont of
    [] -> id
    Exp'' y : k -> \sx -> eval1' y (Add'' : k) sx
    Add'' : k -> \sy -> applyCont1' k (add sy)

eval1' :: Exp -> [Action''] -> Stack -> Stack
eval1' exp k stack =
  case exp of
    Val n -> applyCont1' k (push n stack)
    Add x y -> eval1' x (Exp'' y : k) stack

applyCont2' :: [Action''] -> Stack -> Stack
applyCont2' cont =
  case cont of
    [] -> id
    Exp'' y : k -> \sx -> eval2' (Exp'' y : Add'' : k) sx
    Add'' : k -> \sy -> applyCont2' k (add sy)

eval2' :: [Action''] -> Stack -> Stack
eval2' (Exp'' exp : k) stack =
  case exp of
    Val n -> applyCont2' k (push n stack)
    Add x y -> eval2' (Exp'' x : Exp'' y : k) stack
eval2' _ _ = error "compiler bug!"

-- inlined :: [Action1] -> Stack -> Stack
-- inlined =
--   \case
--     [] -> id
--     Exp1 y : k -> \sx -> eval2 (Exp1 y : Add1 : k) sx
--     Add1 : k -> \sy -> applyCont2 k (add sy)