module Calculating.Compiler.Complicated.Stack.Step0ExplicitStack where

import Calculating.Compiler.Complicated.Stack.Def

eval :: Exp -> Int
eval =
  \case
    Val n -> n
    Bin op x y -> applyOp op (eval x) (eval y)

-- evalS exp stack = eval exp : stack
evalS :: Exp -> Stack -> Stack
evalS exp s =
  case exp of
    Val n -> push n s
    Bin op x y ->
      bin op (evalS y (evalS x s))
