-- https://zhuanlan.zhihu.com/p/643694771
{-# LANGUAGE DeriveFunctor #-}

-- {-# LANGUAGE DeriveApplicative #-}
-- {-# LANGUAGE DeriveMonad #-}

module PE.LetList where

import Control.Monad.ST
import Data.Functor.Const
import Data.STRef
import Data.Text (Text)
import Data.Text qualified as T

mbd :: (Monoid m) => m -> Int -> m
mbd m count
  | count == 0 = mempty
  | count `mod` 2 == 1 = m <> mbd m (count - 1)
  | otherwise = mbd (m <> m) (count `div` 2)

mbdMonad :: (Monoid (m a), Monad m) => m a -> Int -> m a
mbdMonad m count
  | count == 0 = mempty
  | count `mod` 2 == 1 = do
      value <- m
      pure value <> mbd (pure value) (count - 1)
  | otherwise = do
      value <- m <> m
      mbd (pure value) (count `div` 2)

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
newtype Sum = Sum {getSum :: Expr}
  deriving (Show)

newtype SumLetList s a = SumLetList {getSumLetList :: STRef s Int -> STRef s [(Text, Sum)] -> ST s (Const Sum a)}

-- newtype SumLetList s a = SumLetList { getSumLetList :: STRef s Int -> STRef s [(Text, Sum)] -> ST s (Const Sum a) }
-- deriving newtype (Functor, Applicative, Monad)

instance Semigroup (SumLetList s a) where
  (<>) (SumLetList lhs) (SumLetList rhs) = SumLetList $ \counter letList -> do
    lhs' <- lhs counter letList
    rhs' <- rhs counter letList
    index <- readSTRef counter
    let var = T.pack ("$" <> show index)
    modifySTRef' counter (+ 1)
    modifySTRef' letList ((var, getConst (lhs' <> rhs')) :)
    pure (Const (Sum (Var var)))

instance Monoid (SumLetList s a) where
  mempty = SumLetList $ \_ _ -> pure (Const mempty)

newtype Product = Product Expr
  deriving (Show)

instance Semigroup Sum where
  (<>) (Sum lhs) (Sum rhs) = Sum (Add lhs rhs)

instance Monoid Sum where
  mempty = Sum (Int 0)

instance Semigroup Product where
  (<>) (Product lhs) (Product rhs) = Product (Mul lhs rhs)

instance Monoid Product where
  mempty = Product (Int 1)

instance SemiRing Expr where
  zero = Int 0
  one = Int 1
  add = Add
  mul = Mul

demo1 :: Sum
demo1 = mbd (Sum (Var "x")) 13

demo2 :: Sum
demo2 = getConst $ runST $ do
  counter <- newSTRef 0
  letList <- newSTRef []
  getSumLetList (mbd (SumLetList (\_ _ -> pure (Const (Sum (Var "x"))))) 13) counter letList

-- demo2Monad :: Sum
-- demo2Monad = getConst $ runST $ do
--   counter <- newSTRef 0
--   letList <- newSTRef []
--   getSumLetList (mbdMonad (SumLetList (\_ _ -> pure (Const (Sum (Var "x"))))) 13) counter letList
