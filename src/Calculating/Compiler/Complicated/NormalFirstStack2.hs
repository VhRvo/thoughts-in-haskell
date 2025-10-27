{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.NormalFirstStack2 where

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

evalSK' :: Exp -> SCont -> SCont
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

data DSCont1
  = DSId1
  | Bin1O Op Exp DSCont1
  | Bin1I Op DSCont1

applyDSCont :: DSCont1 -> Stack -> Stack
applyDSCont DSId1 s = s
applyDSCont (Bin1O op y k) sx = evalSKD y sx (Bin1I op k)
applyDSCont (Bin1I op k) sy = applyDSCont k (bin op sy)

evalSKD :: Exp -> Stack -> DSCont1 -> Stack
evalSKD exp s k =
  case exp of
    Val n -> applyDSCont k (n : s)
    Bin op x y ->
      evalSKD x s (Bin1O op y k)

data DSAction
  = Bin1OA Op Exp
  | Bin1IA Op

applyDSA :: [DSAction] -> Stack -> Stack
applyDSA [] s = s
applyDSA (Bin1OA op y : k) sx = evalSKDA y sx (Bin1IA op : k)
applyDSA (Bin1IA op : k) sy = applyDSA k (bin op sy)

evalSKDA :: Exp -> Stack -> [DSAction] -> Stack
evalSKDA exp s k =
  case exp of
    Val n -> applyDSA k (n : s)
    Bin op x y ->
      evalSKDA x s (Bin1OA op y : k)

data DSAction2
  = Bin1OA2 Op Exp
  | Bin1OA2' Exp
  | Bin1IA2 Op

applyDSA2 :: [DSAction2] -> Stack -> Stack
applyDSA2 [] s = s
applyDSA2 (Bin1OA2' y : k) sx = evalSKDA2 y sx k
applyDSA2 (Bin1OA2 op y : k) sx = evalSKDA2 y sx (Bin1IA2 op : k)
applyDSA2 (Bin1IA2 op : k) sy = applyDSA2 k (bin op sy)

evalSKDA2 :: Exp -> Stack -> [DSAction2] -> Stack
evalSKDA2 exp s k =
  case exp of
    Val n -> applyDSA2 k (n : s)
    Bin op x y ->
      -- evalSKDA2 x s (Bin1OA2 op y : k)
      evalSKDA2 x s (Bin1OA2' y : Bin1IA2 op : k)

data DSAction'
  = Bin1OA' Exp
  | Bin1IA' Op

applyDSA' :: [DSAction'] -> Stack -> Stack
applyDSA' [] s = s
applyDSA' (Bin1OA' x : k) sx = evalSKDA' x sx k
applyDSA' (Bin1IA' op : k) sy = applyDSA' k (bin op sy)

evalSKDA' :: Exp -> Stack -> [DSAction'] -> Stack
evalSKDA' exp s k =
  case exp of
    Val n -> applyDSA' k (n : s)
    Bin op x y ->
      -- evalSKDA' x s (Bin1OA' y : Bin1IA' op : k)
      applyDSA' (Bin1OA' x : Bin1OA' y : Bin1IA' op : k) s





