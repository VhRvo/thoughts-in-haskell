{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.Stack.Step3Refining where

import Calculating.Compiler.Complicated.Stack.Def

data Action
  = BinLeft Op Exp
  | BinRight Op

type DSCont = [Action]

applyDSCont :: DSCont -> Stack -> Stack
applyDSCont [] s = s
applyDSCont (BinLeft op rhs : k) lhsStack = evalSKD rhs lhsStack (BinRight op : k)
applyDSCont (BinRight op : k) rhsStack = applyDSCont k (bin op rhsStack)

evalSKD :: Exp -> Stack -> DSCont -> Stack
evalSKD exp s k =
  case exp of
    Val n -> applyDSCont k (push n s)
    Bin op x y ->
      evalSKD x s (BinLeft op y : k)
