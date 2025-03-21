module Staging.One where

data Expr
    = Val Int
    | Add Expr Expr

-- 1. If we statically know the expression we can eliminate Expr.
-- 2. If we statically know which Applicative then we can remove the indirection from the typeclass.
eval :: Applicative m => Expr -> m Int
eval = \case
  Val n -> pure n
  Add e1 e2 -> (+) <$> eval e1 <*> eval e2

