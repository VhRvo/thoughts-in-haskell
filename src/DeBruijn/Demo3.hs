{-# LANGUAGE DeriveFunctor #-}

module DeBruijn.Demo3 where

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

zero' :: Term Int
zero' = undefined

succ' :: Term Int
succ' = undefined

env :: Int -> Term Int
env 0 = zero'
env _ = succ'

applySubstitutionEnv :: Term Int -> Term Int
applySubstitutionEnv =
  -- mult (mapWithPolicy shift env term)
  -- multWithPolicy shift
  multWithPolicyShiftEnv Zero

data Nat
  = Zero
  | Succ Nat

-- not tail recursion
applyShift :: Nat -> Int -> Term Int
applyShift Zero         0 = zero'
applyShift Zero         _ = succ'
-- applyShift (Succ times) n = shift (applyShift times) n
applyShift (Succ _)     0 = pure 0
applyShift (Succ times) n = mapWithPolicyLift Zero (applyShift times (n - 1))

multWithPolicyShiftEnv :: Nat -> Term Int -> Term Int
multWithPolicyShiftEnv times =
  \case
    Var x       -> applyShift times x
    Lam body    -> Lam (multWithPolicyShiftEnv (Succ times) body)
    App fun arg -> App (multWithPolicyShiftEnv times fun) (multWithPolicyShiftEnv times arg)

applyLift :: Nat -> Int -> Int
applyLift Zero         n = n + 1
-- applyLift (Succ times) n = lift (applyLift times) n
applyLift (Succ _)     0 = 0
applyLift (Succ times) n = succ (applyLift times (n - 1))

mapWithPolicyLift :: Nat -> Term Int -> Term Int
mapWithPolicyLift times =
  \case
    Var x       -> Var (applyLift times x)
    Lam body    -> Lam (mapWithPolicyLift (Succ times) body)
    App fun arg -> App (mapWithPolicyLift times fun) (mapWithPolicyLift times arg)
