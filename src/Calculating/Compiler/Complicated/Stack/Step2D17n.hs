{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.Stack.Step2D17n where

import Calculating.Compiler.Complicated.Stack.Def

data DSCont
  = Id
  | BinLeft Op Exp DSCont
  | BinRight Op DSCont

applyDSCont :: DSCont -> Stack -> Stack
applyDSCont Id s = s
applyDSCont (BinLeft op rhs k) lhsStack = evalSKD rhs lhsStack (BinRight op k)
applyDSCont (BinRight op k) rhsStack = applyDSCont k (bin op rhsStack)

evalSKD :: Exp -> Stack -> DSCont -> Stack
evalSKD exp s k =
  case exp of
    Val n -> applyDSCont k (push n s)
    Bin op x y ->
      evalSKD x s (BinLeft op y k)
