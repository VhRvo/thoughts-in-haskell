{-# LANGUAGE TemplateHaskell #-}

module Staging.Two where

import Language.Haskell.TH

data Expr
    = Val Int
    | Add Expr Expr

-- The eval function takes an expression and generates code which unrolls the expression that needs to be evaluated.
-- Splicing in eval gives us a chain of additions which are computed at run-time.
-- $$(eval (Add (Val 1) (Val 2))) => 1 + 2
-- By explicitly separating the program into stages we know that there will be no mention of Expr in the resulting program.
eval :: (Quote m) => Expr -> Code m Int
eval = \case
  Val n -> [|| n ||]
  Add e1 e2 -> [|| $$(eval e1) + $$(eval e2) ||]

