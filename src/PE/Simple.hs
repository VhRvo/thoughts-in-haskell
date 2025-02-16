-- https://zhuanlan.zhihu.com/p/643694771

module PE.Simple where

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

