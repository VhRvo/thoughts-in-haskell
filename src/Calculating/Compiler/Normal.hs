module Calculating.Compiler.Normal where

data Exp
  = Val Int
  | Add Exp Exp

eval :: Exp -> Int
eval =
  \case
    Val n -> n
    Add x y -> eval x + eval y

evalK :: Exp -> (Int -> Int) -> Int
evalK exp k =
  case exp of
    Val n -> k n
    Add x y -> evalK x (\x' -> evalK y (\y' -> k (x' + y')))

data Cont
  = Id
  | OAdd Exp Cont
  | IAdd Int Cont

evalK1 :: Exp -> Cont -> Int
evalK1 exp k =
  case exp of
    Val n -> applyCont k n
    Add x y -> evalK1 x (OAdd y k)

applyCont :: Cont -> Int -> Int
applyCont =
  \case
    Id -> id
    OAdd y k -> \x' -> evalK1 y (IAdd x' k)
    IAdd x' k -> \y' -> applyCont k (x' + y')

data Action
  = OAdd' Exp
  | IAdd' Int

type Cont' = [Action]

applyCont' :: Cont' -> Int -> Int
applyCont' =
  \case
    [] -> id
    OAdd' y : k -> \x' -> evalK2 y (IAdd' x' : k)
    IAdd' x' : k -> \y' -> applyCont' k (x' + y')

evalK2 :: Exp -> Cont' -> Int
evalK2 exp k =
  case exp of
    Val n -> applyCont' k n
    Add x y -> evalK2 x (OAdd' y : k)

data Tag
  = Value Int
  | Exp Exp

-- step :: Tag -> Cont' -> Int
-- step =
--   \case
--     Value



