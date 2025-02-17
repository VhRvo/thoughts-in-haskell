-- https://zhuanlan.zhihu.com/p/643694771

-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveApplicative #-}
-- {-# LANGUAGE DeriveMonad #-}
-- {-# LANGUAGE FlexibleInstances #-}

module PE.StateLetList where

import Control.Monad.ST
import Control.Monad.State
import Data.Functor.Const
import Data.Functor.Identity
import Data.STRef
import Data.Text (Text)
import Data.Text qualified as T

mbd :: (Monoid m) => m -> Int -> m
mbd m count
  | count == 0 = mempty
  | count `mod` 2 == 1 = m <> mbd m (count - 1)
  | otherwise = mbd (m <> m) (count `div` 2)

-- This is actually pure implementation.
-- Every (<>) operations is pure.
mbdPure' :: (Monoid a, Monoid (m a), Monad m) => m a -> Int -> m a
mbdPure' impure count
  | count == 0 = pure mempty
  | count `mod` 2 == 1 = do
      value <- impure
      (value <>) <$> mbdPure' (pure value) (count - 1)
  | otherwise = do
      value <- impure
      mbdPure' (pure (value <> value)) (count `div` 2)

mbdImpure' :: (Monoid a, Monoid (m a), Monad m) => m a -> Int -> m a
mbdImpure' impure count
  | count == 0 = pure mempty
  | count `mod` 2 == 1 = do
      value <- impure
      (value <>) <$> mbdImpure' (pure value) (count - 1)
  | otherwise = do
      value <- impure
      mbdImpure' (pure value <> pure value) (count `div` 2)

mbdImpure :: (Monoid (m a), Monad m) => m a -> Int -> m a
mbdImpure impure count
  | count == 0 = mempty
  | count `mod` 2 == 1 = do
      value <- impure
      pure value <> mbdImpure (pure value) (count - 1)
  | otherwise = do
      value <- impure
      mbdImpure (pure value <> pure value) (count `div` 2)

class SemiRing a where
  zero :: a
  one :: a
  add :: a -> a -> a
  mul :: a -> a -> a

data Expr
  = Var Text
  | Int Int
  | Add Expr Expr
  | Mul Expr Expr
  | Let Text Expr Expr
  deriving (Show)

newtype Sum = Sum {getSum :: Expr}
  deriving (Show)

instance Semigroup Sum where
  (<>) (Sum lhs) (Sum rhs) = Sum (Add lhs rhs)

instance Monoid Sum where
  mempty = Sum (Int 0)

class Sharable a where
  share :: Text -> a

instance Sharable Sum where
  share var = Sum (Var var)

newtype SumLetList a = SumLetList {getSumLetList :: State (Int, [(Text, Sum)]) a}
  deriving newtype (Functor, Applicative, Monad)

instance (a ~ Sum, Semigroup a, Sharable a) => Semigroup (SumLetList a) where
  (<>) (SumLetList lhs) (SumLetList rhs) = SumLetList $ do
    lhs' <- lhs
    rhs' <- rhs
    (index, letList) <- get
    let var = T.pack ("$" <> show index)
    put (index + 1, (var, lhs' <> rhs') : letList)
    pure (share var)

instance (a ~ Sum, Monoid a, Sharable a) => Monoid (SumLetList a) where
  mempty = SumLetList $ pure mempty

-- demo1 :: Sum
-- demo1 = mbd (Sum (Var "x")) 13

-- demo2 :: Sum
-- demo2 = getConst $ runST $ do
--   counter <- newSTRef 0
--   letList <- newSTRef []
--   getSumLetList (mbd (SumLetList (\_ _ -> pure (Const (Sum (Var "x"))))) 13) counter letList

demo2Impure :: (Sum, (Int, [(Text, Sum)]))
demo2Impure =
  runIdentity
    ( (`runStateT` (1, []))
        (getSumLetList (mbdImpure (SumLetList (pure (Sum (Var "x")))) 13))
    )

demo2Impure' :: (Sum, (Int, [(Text, Sum)]))
demo2Impure' =
  runIdentity
    ( (`runStateT` (1, []))
        (getSumLetList (mbdImpure' (SumLetList (pure (Sum (Var "x")))) 13))
    )
