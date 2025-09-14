{-# LANGUAGE DeriveFunctor #-}

module DeBruijn.Demo2 where

import Prelude hiding (succ)

data Term a
  = Var a
  | Lam (Term a)
  | App (Term a) (Term a)
  deriving (Eq, Ord, Show)
  deriving (Functor)

instance Applicative Term where
  pure = Var
  Var f <*> term = f <$> term
  Lam body <*> term = Lam (body <*> term)
  App fun arg <*> term = App (fun <*> term) (arg <*> term)

instance Monad Term where
  Var x >>= f = f x
  Lam body >>= f = Lam (body >>= f)
  App fun arg >>= f = App (fun >>= f) (arg >>= f)

mult :: Term (Term a) -> Term a
mult = \case
  Var term -> term
  Lam body -> Lam (mult body)
  App fun arg -> App (mult fun) (mult arg)

mapWithPolicy :: ((a -> b) -> (a -> b)) -> (a -> b) -> Term a -> Term b
mapWithPolicy z f =
  \case
    Var x       -> Var (f x)
    Lam body    -> Lam (mapWithPolicy z (z f) body)
    App fun arg -> App (mapWithPolicy z f fun) (mapWithPolicy z f arg)

multWithPolicy :: ((a -> Term b) -> (a -> Term b)) -> (a -> Term b) -> Term a -> Term b
multWithPolicy z f =
  \case
    Var x       -> f x
    Lam body    -> Lam (multWithPolicy z (z f) body)
    App fun arg -> App (multWithPolicy z f fun) (multWithPolicy z f arg)

succ :: Int -> Int
succ = (1 +)

lift :: (Int -> Int) -> Int -> Int
lift f n
  | n == 0    = 0
  | otherwise = 1 + f (n - 1)
              -- ^        ^ decrement out of lambda
              -- | increment under lambda

shift :: (Int -> Term Int) -> Int -> Term Int
shift env n
  | n == 0    = pure 0
  | otherwise =
    -- mapWithPolicy lift succ (env (n - 1))
    mapWithPolicyLift Zero (env (n - 1))

applySubstitution :: (Int -> Term Int) -> Term Int -> Term Int
applySubstitution =
  -- mult (mapWithPolicy shift env term)
  -- multWithPolicy shift
  multWithPolicyShift Zero

data Nat
  = Zero
  | Succ Nat

applyShift :: Nat -> (Int -> Term Int) -> Int -> Term Int
applyShift Zero         env = env
-- not tail recursion
applyShift (Succ times) env = shift (applyShift times env)
-- tail recursion
-- applyShift (Succ times) env = applyShift times (shift env)

multWithPolicyShift :: Nat -> (Int -> Term Int) -> Term Int -> Term Int
multWithPolicyShift times env =
  \case
    Var x       -> applyShift times env x
    Lam body    -> Lam (multWithPolicyShift (Succ times) env body)
    App fun arg -> App (multWithPolicyShift times env fun) (multWithPolicyShift times env arg)

applyLift :: Nat -> Int -> Int
applyLift Zero         = succ
applyLift (Succ times) = lift (applyLift times)

-- tail recursion
-- applyLift :: Nat -> (Int -> Int) -> Int -> Int
-- applyLift Zero         succ = succ
-- applyLift (Succ times) succ = applyLift times (lift succ)

mapWithPolicyLift :: Nat -> Term Int -> Term Int
mapWithPolicyLift times =
  \case
    Var x       -> Var (applyLift times x)
    Lam body    -> Lam (mapWithPolicyLift (Succ times) body)
    App fun arg -> App (mapWithPolicyLift times fun) (mapWithPolicyLift times arg)
