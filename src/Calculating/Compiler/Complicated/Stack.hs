{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.Stack where

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

type Stack = [Int]

push :: Int -> Stack -> Stack
push = (:)

bin :: Op -> Stack -> Stack
bin op (n : m : s) = applyOp op m n : s
bin _ _ = error "compiler bug"

evalS :: Exp -> Stack -> Stack
evalS exp s =
  case exp of
    Val n -> push n s
    Bin op x y ->
      bin op (evalS y (evalS x s))

type SCont = Stack -> Stack

evalSK' :: Exp -> (Stack -> Stack) -> Stack -> Stack
evalSK' exp k =
  case exp of
    Val n -> k . push n
    Bin op x y ->
      evalSK' x (evalSK' y (k . bin op))

evalSK :: Exp -> Stack -> (Stack -> Stack) -> Stack
evalSK exp s k =
  case exp of
    Val n -> k (n : s)
    Bin op x y ->
      evalSK x s (\sx ->
        evalSK y sx (\sy ->
          k (bin op sy)))

data DSCont
  = Id
  | BinLeft Op Exp DSCont
  | BinRight Op DSCont

applyDSCont :: DSCont -> Stack -> Stack
applyDSCont Id s = s
applyDSCont (BinLeft op rhs k) lhsStack = evalSKD rhs lhsStack (BinRight op k)
applyDSCont (BinRight op k) rhsStack = applyDSCont k (bin op rhsStack)

evalSKD :: Exp -> Stack -> DSCont -> Stack
evalSKD exp s k =
  case exp of
    Val n -> applyDSCont k (n : s)
    Bin op x y ->
      evalSKD x s (BinLeft op y k)

data DAction1
  = DABinLeft Op Exp
  | DABinRight Op

type DSCont1 = [DAction1]

applyDSCont1 :: DSCont1 -> Stack -> Stack
applyDSCont1 [] s = s
applyDSCont1 (DABinLeft op rhs : k) lhsStack = evalSKDA rhs lhsStack (DABinRight op : k)
applyDSCont1 (DABinRight op : k) rhsStack = applyDSCont1 k (bin op rhsStack)

evalSKDA :: Exp -> Stack -> DSCont1 -> Stack
evalSKDA exp s k =
  case exp of
    Val n -> applyDSCont1 k (n : s)
    Bin op x y ->
      evalSKDA x s (DABinLeft op y : k)

data DAction1
  = DABinLeft Op Exp
  | DABinRight Op

type DSCont1 = [DAction1]

applyDSCont1 :: DSCont1 -> Stack -> Stack
applyDSCont1 [] s = s
applyDSCont1 (DABinLeft op rhs : k) lhsStack = evalSKDA rhs lhsStack (DABinRight op : k)
applyDSCont1 (DABinRight op : k) rhsStack = applyDSCont1 k (bin op rhsStack)

evalSKDA :: Exp -> Stack -> DSCont1 -> Stack
evalSKDA exp s k =
  case exp of
    Val n -> applyDSCont1 k (n : s)
    Bin op x y ->
      evalSKDA x s (DABinLeft op y : k)
