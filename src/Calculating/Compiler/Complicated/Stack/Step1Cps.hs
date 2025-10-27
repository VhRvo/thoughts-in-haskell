{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.Stack.Step1Cps where

import Calculating.Compiler.Complicated.Stack.Def

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
