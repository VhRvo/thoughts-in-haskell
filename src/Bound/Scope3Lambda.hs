{-# LANGUAGE DeriveTraversable #-}
module Bound.Scope3Lambda where

import Bound.Scope3
import Data.Functor.Classes
import Control.Monad (ap)

data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Lam (Scope () Exp a)
--   deriving (Eq, Ord, Show)
  deriving (Functor, Foldable, Traversable)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  Var a       >>= f = f a
  App fun arg >>= f = App (fun >>= f) (arg >>= f)
  Lam body    >>= f = Lam (body >>>= f)

-- instance Eq1 Exp

-- whnf :: forall a. Exp a -> Exp a
-- whnf (App fun arg) =
--   case whnf fun of
--     Lam body -> whnf (instantiate (const arg) body)
--     f' -> App f' arg
-- whnf expr = expr

-- lam :: forall a. (Eq a) => a -> Exp a -> Exp a
-- lam var body = Lam (abstract (\var' -> if var == var' then Just () else Nothing) body)

-- nf :: Exp a -> Exp a
-- nf e@Var{} = e
-- nf (Lam body) = Lam $ toScope $ nf $ fromScope body
-- nf (App fun arg) = case whnf f of
--   Lam body -> nf (instantiate (const arg) body)
--   fun' -> nf fun' `App` nf arg

