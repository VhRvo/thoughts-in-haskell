{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.Stack.Step6Refining2 where

import Calculating.Compiler.Complicated.Stack.Def

data Action
  = Exp Exp
  | Op Op

type DSCont = [Action]

step :: Action -> DSCont -> Stack -> (DSCont, Stack)
step (Exp (Val n)) k s =
  (k, push n s)
step (Exp (Bin op x y)) k s =
  (Exp x : Exp y : Op op : k, s)
step (Op op) k s =
  (k, bin op s)

drivier :: (DSCont, Stack) -> Stack
drivier ([], s) = s
drivier (action : k, s) = drivier (step action k s)
