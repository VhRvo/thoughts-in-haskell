{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.Stack.Step5Unified where

import Calculating.Compiler.Complicated.Stack.Def

data Action
  = Exp Exp
  | Op Op

type DSCont = [Action]

step :: DSCont -> Stack -> Stack
step [] s = s
step (Exp (Val n) : k) s =
  step k (push n s)
step (Exp (Bin op x y) : k) s =
  step (Exp x : Exp y : Op op : k) s
step (Op op : k) s =
  step k (bin op s)
