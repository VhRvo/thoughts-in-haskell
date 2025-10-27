{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.Stack.Step6Refining where

import Calculating.Compiler.Complicated.Stack.Def

data Action
  = Exp Exp
  | Op Op

type DSCont = [Action]

step :: DSCont -> Stack -> (DSCont, Stack)
step (Exp (Val n) : k) s =
  (k, push n s)
step (Exp (Bin op x y) : k) s =
  (Exp x : Exp y : Op op : k, s)
step (Op op : k) s =
  (k, bin op s)
step [] _ = error "compiler bug!"

drivier :: (DSCont, Stack) -> Stack
drivier ([], s) = s
drivier (k, s) = drivier (step k s)
