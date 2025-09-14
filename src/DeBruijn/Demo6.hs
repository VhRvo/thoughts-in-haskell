{-# LANGUAGE DeriveFunctor #-}

module DeBruijn.Demo6 where

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
    Var x -> Var (f x)
    Lam body -> Lam (mapWithPolicy z (z f) body)
    App fun arg -> App (mapWithPolicy z f fun) (mapWithPolicy z f arg)

multWithPolicy :: ((a -> Term b) -> (a -> Term b)) -> (a -> Term b) -> Term a -> Term b
multWithPolicy z f =
  \case
    Var x -> f x
    Lam body -> Lam (multWithPolicy z (z f) body)
    App fun arg -> App (multWithPolicy z f fun) (multWithPolicy z f arg)

succ :: Int -> Int
succ = (1 +)

lift :: (Int -> Int) -> Int -> Int
lift f n
  | n == 0 = 0
  | otherwise = 1 + f (n - 1)

-- \^        ^ decrement out of lambda
-- \| increment under lambda

shift :: (Int -> Term Int) -> Int -> Term Int
shift env n
  | n == 0 = pure 0
  | otherwise =
      -- mapWithPolicy lift succ (env (n - 1))
      mapWithPolicyLift 0 (env (n - 1))

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
  multWithPolicyShiftEnv 0

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ = id
nTimes n f = nTimes (n - 1) f . f

-- not tail recursion
-- applyShift :: Int -> Int -> Term Int
-- applyShift 0     0 = zero'
-- applyShift 0     _ = succ'
-- -- applyShift (Succ times) n = shift (applyShift times) n
-- applyShift _     0 = pure 0
-- applyShift times n = mapWithPolicyLift 0 (applyShift (times - 1) (n - 1))
-- conditional version
-- applyShift :: Int -> Int -> Term Int
-- applyShift times n
--   | times > n  = nTimes n     (mapWithPolicyLift 0) (pure 0)
--   | times == n = nTimes n     (mapWithPolicyLift 0) zero'
--   | otherwise  = nTimes times (mapWithPolicyLift 0) succ'
applyShift :: Int -> Int -> Term Int
applyShift times n
  | times > n = mapWithPolicyLiftNTimes n 0 (Var 0)
  | times == n = mapWithPolicyLiftNTimes n 0 zero'
  | otherwise = mapWithPolicyLiftNTimes times 0 succ'

multWithPolicyShiftEnv :: Int -> Term Int -> Term Int
multWithPolicyShiftEnv times =
  \case
    Var x -> applyShift times x
    Lam body -> Lam (multWithPolicyShiftEnv (1 + times) body)
    App fun arg -> App (multWithPolicyShiftEnv times fun) (multWithPolicyShiftEnv times arg)

-- applyLift :: Int -> Int -> Int
-- applyLift 0     0 = 1
-- applyLift 0     n = n + 1
-- -- applyLift (Succ times) n = lift (applyLift times) n
-- applyLift _     0 = 0
-- applyLift times n = succ (applyLift (times - 1) (n - 1))
-- applyLift :: Int -> Int -> Int
-- applyLift times n
--   | times > n  = nTimes n     succ 0
--   | times == n = nTimes n     succ 1
--   | otherwise  = nTimes times succ (n - times + 1)
applyLift :: Int -> Int -> Int
applyLift times n
  | times > n = n
  | otherwise = n + 1

applyLiftNTimes :: Int -> Int -> Int -> Int
applyLiftNTimes outer inner x
  | inner > x = x
  | otherwise = x + outer

mapWithPolicyLift :: Int -> Term Int -> Term Int
mapWithPolicyLift times =
  \case
    Var x -> Var (applyLift times x)
    Lam body -> Lam (mapWithPolicyLift (1 + times) body)
    App fun arg -> App (mapWithPolicyLift times fun) (mapWithPolicyLift times arg)

mapWithPolicyLiftNTimes :: Int -> Int -> Term Int -> Term Int
mapWithPolicyLiftNTimes outer inner =
  \case
    Var x -> Var (nTimes outer (applyLift inner) x)
    Lam body -> Lam (mapWithPolicyLiftNTimes outer (1 + inner) body)
    App fun arg ->
      App
        (mapWithPolicyLiftNTimes outer inner fun)
        (mapWithPolicyLiftNTimes outer inner arg)
