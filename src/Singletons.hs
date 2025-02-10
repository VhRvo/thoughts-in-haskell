{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE NoTypeInType #-}

module Singletons where

import Data.Kind (Type, Constraint)
import Prelude hiding (head, tail)

-- data Nat = Zero | Succ Nat
type Nat :: Type
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

type SNat :: Nat -> Type
-- data SNat :: Nat -> Type where
data SNat num where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

type Pred :: Nat -> Nat
type family Pred n where
  Pred Zero = Zero
  Pred (Succ n) = n

sPred :: SNat n -> SNat (Pred n)
sPred SZero = SZero
sPred (SSucc n) = n

sSucc :: SNat n -> SNat (Succ n)
sSucc = SSucc

one :: SNat (Succ Zero)
one = SSucc SZero


data family Sing (a :: k)
data instance Sing (list :: [k]) where
    SNil :: Sing '[]
    SCons :: Sing head -> Sing tail -> Sing (head ': tail)

-- newtype instance Sing (list :: [k]) = SSList (SList list)
-- type SList :: forall k. [k] -> Type
-- data SList xs where
--   SNil :: SList '[]
--   SCons :: Sing head -> SList tail -> SList (head ': tail)

type Map :: forall k1 k2. (k1 -> k2)  -> [k1]  -> [k2]
type family Map f xs where
    Map f '[] = '[]
    Map f (head ': tail) = f head ': Map f tail

sMap :: (forall a. Sing a -> Sing (f a)) -> Sing list -> Sing (Map f list)
sMap _ SNil = SNil
sMap f (SCons head tail) = SCons (f head) (sMap f tail)

sOne :: SNat (Succ Zero)
sOne   = SSucc SZero
sTwo :: SNat (Succ (Succ Zero))
sTwo   = SSucc sOne
sThree :: SNat (Succ (Succ (Succ Zero)))
sThree = SSucc sTwo

-- sNumbers  = SCons sThree SNil -- [3]
-- sNumbers  = SCons sOne $ SCons sTwo $ SCons sThree SNil -- [1,2,3]

-- twoThreeFour = sMap sSucc sNumbers


-- 1st version
-- type family Apply (f :: k1 -> k2) (a :: k1) :: k2

-- 2nd version
-- type family Apply (f :: Type) (a :: k1) :: k2
-- type Apply :: forall k1 k2. Type -> k1 -> k2
-- type family Apply f a

-- type PredSym :: Type
-- data PredSym
-- type instance Apply PredSym n = Pred n

-- type ZeroSNat :: Nat
-- type ZeroSNat = Apply PredSym (Succ Zero)
-- type BadSNat = Apply PredSym '[Int, Bool] -- stuck due to kind mismatch

-- type TyFun :: forall k1 k2. k1 -> k2 -> Type
type TyFun :: forall k1 k2. k1 -> k2 -> Type
-- data TyFun :: forall k1 k2. k1 -> k2 -> Type
data TyFun k1 k2
-- data TyFun
-- data TyFun :: Type -> Type -> Type
-- 3rd version
-- type family Apply (f :: TyFun k1 k2) (x :: k1) :: k2

-- > • Data type has non-Type return kind ‘TyFun Nat Nat’
-- > • In the data declaration for ‘PredSym’
-- data PredSym :: TyFun Nat Nat
-- type instance Apply PredSym x = Pred x

-- 4th version
-- type Apply :: (TyFun k1 k2 -> Type) -> k1 -> k2
type Apply :: forall k1 k2. (TyFun k1 k2 -> Type) -> k1 -> k2
-- type Apply :: forall k1 k2. (Type -> Type) -> k1 -> k2 -- bad
-- type family Apply f x
-- type Apply :: forall k1 k2. (Type -> Type) -> k1 -> k2
-- type family Apply f x :: k2

-- type Apply :: forall (t1 :: Type) (t2 :: Type). (TyFun t1 t2 -> Type) -> Type -> Type
type family Apply f x
-- type family Apply (f :: TyFun t1 t2 -> Type) (x :: Type) :: Type
-- type family Apply (f :: TyFun t1 t2 -> Type) (x :: t1) :: t2
data PredSym :: TyFun Nat Nat -> Type
type instance Apply PredSym x = Pred x

-- type ZeroSNat :: Nat
-- type ZeroSNat = Apply PredSym (Succ (Succ Zero))
-- type BadSNat = Apply PredSym '[Int, Bool]

-- legal
data Test :: Constraint -> Nat -> Type
