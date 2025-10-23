{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.NormalFirstCps where

data Exp
  = Val Int
  | Bin Op Exp Exp

data Op
  = Add
  | Mul

applyOp :: Op -> Int -> Int -> Int
applyOp Add = (+)
applyOp Mul = (*)

applyOpK :: Op -> Int -> Int -> (Int -> Int) -> Int
applyOpK Add l r k = k (l + r)
applyOpK Mul l r k = k (l * r)

eval :: Exp -> Int
eval =
  \case
    Val n -> n
    Bin op x y -> applyOp op (eval x) (eval y)

evalK :: Exp -> (Int -> Int) -> Int
evalK exp k =
  case exp of
    Val n -> k n
    Bin op x y -> evalK x (\x' -> evalK y (\y' -> applyOpK op x' y' k))

data Cont
  = Id
  | OBin Op Exp Cont
  | IBin Op Int Cont

applyCont :: Cont -> Int -> Int
applyCont =
  \case
    Id -> id
    OBin op y k -> \x' -> evalDefun y (IBin op x' k)
    IBin op x' k -> \y' -> applyCont k (applyOp op x' y')

evalDefun :: Exp -> Cont -> Int
evalDefun exp k =
  case exp of
    Val n -> applyCont k n
    Bin op x y -> evalDefun x (OBin op y k)

data Action'
  = OBin' Op Exp
  | IBin' Op Int

applyCont1 :: [Action'] -> Int -> Int
applyCont1 =
  \case
    [] -> id
    OBin' op y : k -> \x' -> evalDefun1 y (IBin' op x' : k)
    IBin' op x' : k -> \y' -> applyCont1 k (applyOp op x' y')

evalDefun1 :: Exp -> [Action'] -> Int
evalDefun1 exp k =
  case exp of
    Val n -> applyCont1 k n
    Bin op x y -> evalDefun1 x (OBin' op y : k)

data Tag
  = TExp Exp
  | TInt Int

step :: ([Action'], Tag) -> ([Action'], Tag)
step ([], TInt n)                =  ([], TInt n)
step (OBin' op y  : k, TInt x')  =  step (IBin' op x' : k, TExp y)
step (IBin' op x' : k, TInt y')  =  step (k, TInt (applyOp op x' y'))
step (k, TExp (Val n))           =  step (k, TInt n)
step (k, TExp (Bin op x y))      =  step (OBin' op y : k, TExp x)

-- applyCont2 :: [Action'] -> Int -> [Int] -> [Int]
-- applyCont2 =
--   \case
--     [] -> (:)
--     OBin' op y : k -> \x' -> evalDefun2 y (IBin' op x' : k)
--     IBin' op x' : k -> \y' -> applyCont2 k (applyOp op x' y')

-- evalDefun2 :: Exp -> [Action'] -> [Int] -> [Int]
-- evalDefun2 exp k =
--   case exp of
--     Val n -> applyCont2 k n
--     Bin op x y -> evalDefun2 x (OBin' op y : k)
