{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.Stack.Step4Refining where

import Calculating.Compiler.Complicated.Stack.Def

data Action
  = Exp Exp
  | Op Op

type DSCont = [Action]

applyDSCont :: DSCont -> Stack -> Stack
applyDSCont [] s = s
applyDSCont (Exp rhs : k) lhsStack = evalSKD rhs lhsStack k
applyDSCont (Op op : k) rhsStack = applyDSCont k (bin op rhsStack)

evalSKD :: Exp -> Stack -> DSCont -> Stack
evalSKD exp s k =
  case exp of
    Val n -> applyDSCont k (push n s)
    Bin op x y ->
      -- evalSKD x s (Exp y : Op op : k)
      applyDSCont (Exp x : Exp y : Op op : k) s
