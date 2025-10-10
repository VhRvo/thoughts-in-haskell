{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module CPS.Traversal.V1 where

import CPS.Traversal.Syntax

type CVar = Var

data CExp
  = CVar CVar
  | CLam CVar CExp
  | CApp CExp CExp

transform :: Exp -> CExp
transform =
  \case
    Var var -> CVar var
    Lam var body -> CLam var (transform body)
    App fun arg -> CApp (transform fun) (transform arg)

transformK1 :: forall r. Exp -> (CExp -> r) -> r
transformK1 exp k =
  case exp of
    Var var -> k (CVar var)
    Lam var body -> k (CLam var (transform body))
    App fun arg -> k (CApp (transform fun) (transform arg))

transformK2 :: Exp -> (CExp -> CExp) -> CExp
transformK2 exp k =
  case exp of
    Var var -> k (CVar var)
    Lam var body -> k (CLam var (transformK2 body id))
    App fun arg -> transformK2 fun (\fun' -> transformK2 arg (\arg' -> k (CApp fun' arg')))



