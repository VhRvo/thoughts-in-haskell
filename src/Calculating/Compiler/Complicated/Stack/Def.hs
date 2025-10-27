{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.Stack.Def where

data Exp
  = Val Int
  | Bin Op Exp Exp

data Op
  = Add
  | Mul

applyOp :: Op -> Int -> Int -> Int
applyOp Add = (+)
applyOp Mul = (*)

type Stack = [Int]

push :: Int -> Stack -> Stack
push = (:)

bin :: Op -> Stack -> Stack
bin op (n : m : s) = applyOp op m n : s
bin _ _ = error "compiler bug"
