-- https://zhuanlan.zhihu.com/p/643694771

-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveApplicative #-}
-- {-# LANGUAGE DeriveMonad #-}
{-# LANGUAGE FlexibleInstances #-}

module PE.StateLetList where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.ST
import Data.STRef
import Data.Functor.Const
import Control.Monad.State
import Data.Functor.Identity

mbd :: (Monoid m) => m -> Int -> m
mbd m count
  | count == 0 = mempty
  | count `mod` 2 == 1 = m <> mbd m (count - 1)
  | otherwise = mbd (m <> m) (count `div` 2)

-- mbdImpure :: (Monoid (m a), Monad m) => m a -> Int -> m a
-- mbdImpure impure count
--   | count == 0 = pure mempty
--   | count `mod` 2 == 1 = do
--       value <- impure
--       (value <>) <$> mbdImpure (pure value) (count - 1)
--   | otherwise = do
--       value <- impure
--       mbdImpure (pure (value <> value)) (count `div` 2)

mbdImpure :: (Monoid (m a), Monad m) => m a -> Int -> m a
mbdImpure impure count
  | count == 0 = mempty
  | count `mod` 2 == 1 = do
      value <- impure
      pure value <> mbdImpure (pure value) (count - 1)
      -- (value <>) <$> mbdImpure (pure value) (count - 1)
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

-- newtype Sum a = Sum a
-- newtype Product a = Product a
newtype Sum = Sum { getSum :: Expr }
  deriving (Show)

instance Semigroup Sum where
    (<>) (Sum lhs) (Sum rhs) = Sum (Add lhs rhs)

instance Monoid Sum where
    mempty = Sum (Int 0)

newtype SumLetList a = SumLetList { getSumLetList :: State (Int, [(Text, Sum)]) a }
  deriving newtype (Functor, Applicative, Monad)

-- instance (Semigroup a) => Semigroup (SumLetList a) where
--   (<>) (SumLetList lhs) (SumLetList rhs) = SumLetList $ do
--     lhs' <- lhs
--     rhs' <- rhs
--     (index, letList) <- get
--     let var = T.pack ("$" <> show index)
--     put (index + 1, (var, lhs' <> rhs') : letList)
--     pure (Sum (Var var))

-- instance (Monoid a) => Monoid (SumLetList a) where
--   mempty = SumLetList $ pure mempty

instance Semigroup (SumLetList Sum) where
  (<>) (SumLetList lhs) (SumLetList rhs) = SumLetList $ do
    lhs' <- lhs
    rhs' <- rhs
    (index, letList) <- get
    let var = T.pack ("$" <> show index)
    put (index + 1, (var, lhs' <> rhs') : letList)
    pure (Sum (Var var))

instance Monoid (SumLetList Sum) where
  mempty = SumLetList $ pure mempty

-- newtype Product = Product Expr
--   deriving (Show)

-- instance Semigroup Product where
--     (<>) (Product lhs) (Product rhs) = Product (Mul lhs rhs)

-- instance Monoid Product where
--     mempty = Product (Int 1)

-- instance SemiRing Expr where
--     zero = Int 0
--     one = Int 1
--     add = Add
--     mul = Mul

-- demo1 :: Sum
-- demo1 = mbd (Sum (Var "x")) 13

-- demo2 :: Sum
-- demo2 = getConst $ runST $ do
--   counter <- newSTRef 0
--   letList <- newSTRef []
--   getSumLetList (mbd (SumLetList (\_ _ -> pure (Const (Sum (Var "x"))))) 13) counter letList

demo2Monad :: (Sum, (Int, [(Text, Sum)]))
demo2Monad =
  runIdentity ((`runStateT` (1, [])) (getSumLetList (mbdImpure (SumLetList (pure (Sum (Var "x")))) 13)))
