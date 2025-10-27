module Calculating.Compiler.Complicated.Stack.Normal where

import Calculating.Compiler.Complicated.Stack.Def

eval :: Exp -> Int
eval =
  \case
    Val n -> n
    Bin op x y -> applyOp op (eval x) (eval y)
