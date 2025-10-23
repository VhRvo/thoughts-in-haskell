{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.Normal where

data Exp
  = Val Int
  | Bin Op Exp Exp

data Op
  = Add
  | Mul

applyOp :: Op -> Int -> Int -> Int
applyOp Add = (+)
applyOp Mul = (*)

eval :: Exp -> Int
eval =
  \case
    Val n -> n
    Bin op x y -> applyOp op (eval x) (eval y)

evalK :: Exp -> (Int -> Int) -> Int
evalK exp k =
  case exp of
    Val n -> k n
    Bin op x y -> evalK x (\x' -> evalK y (\y' -> k (applyOp op x' y')))

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
