-- https://zhuanlan.zhihu.com/p/643694771

module PE.Simple where

import Data.Text (Text)
import qualified Data.Text as T

mbd :: (Monoid m) => m -> Int -> m
mbd m count
  | count == 0 = mempty
  | count `mod` 2 == 1 = m <> mbd m (count - 1)
  | otherwise = mbd (m <> m) (count `div` 2)

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
  deriving (Show)

-- newtype Sum a = Sum a
-- newtype Product a = Product a
newtype Sum = Sum Expr
  deriving (Show)
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

demo :: Sum
demo = mbd (Sum (Var "x")) 13

