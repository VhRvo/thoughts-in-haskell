{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module OneTypeFamilyData where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import GHC.TypeNats (Nat, type (+))

type family Fst' (xy :: (a, b)) :: a

type instance Fst' '(x, y) = x

type Expr :: Type -> Type
-- type Expr a = a -> Type
type Expr a = a -> Type
-- data Expr x :: Type

type Eval :: Expr a -> a
type family Eval (e :: Expr a) :: a

--   fst ::     (a, b)  ->     a
-- data Fst (xy :: (a, b)) :: Expr a
-- type Fst :: forall k (a :: k) (b :: k). (a, b) -> Expr a
type Fst :: forall (a :: Type) (b :: Type). (a, b) -> Expr a
data Fst xy :: Expr a

-- data Third (xy :: (a, b)) :: Expr b

type instance Eval (Fst '(x, y)) = x

--   snd :: (a, b) ->     b
-- type Snd :: (a, b) -> Expr b
-- data Snd xy :: Expr b
type Snd :: forall (a :: Type) (b :: Type). (a, b) -> Expr b
data Snd xy :: Expr b

-- data Snd xy y

type instance Eval (Snd '(x, y)) = y

--   fromMaybe :: a -> Maybe a ->     a
type FromMaybe :: forall (a :: Type). a -> Maybe a -> Expr a
data FromMaybe :: a -> Maybe a -> Expr a

-- data FromMaybe x mx :: Expr x1

type instance Eval (FromMaybe x0 'Nothing) = x0

type instance Eval (FromMaybe _ ('Just x)) = x

type Length' :: forall (a :: Type). [a] -> Nat
type family Length' (xs :: [a]) :: Nat

type instance Length' '[] = 0

type instance Length' (x ': xs) = 1 + Length' xs

type Length :: forall (a :: Type). [a] -> Expr a
data Length xs :: Expr a

type instance Eval (Length '[]) = 0

type instance Eval (Length (_ ': xs)) = 1 + Eval (Length xs)

-- traverse :: Applicative m => (a -> m b) -> [a] -> m [b]
type Map :: forall (a :: Type) (b :: Type). (a -> Expr b) -> [a] -> Expr [b]
data Map f xs :: Expr [b]

type instance Eval (Map f '[]) = '[]

type instance Eval (Map f (x ': xs)) = Eval (f x) : Eval (Map f xs)

type (>>=) :: forall (a :: Type) (b :: Type). Expr a -> (a -> Expr b) -> Expr b
data (>>=) mx f :: Expr b
type instance Eval (mx >>= f) = Eval (f (Eval mx))

type Pure :: forall (a :: Type). a -> Expr a
data Pure x :: Expr a
type instance Eval (Pure x) = x

type (>=>) :: forall (a :: Type) (b :: Type) (c :: Type). (a -> Expr b) -> (b -> Expr c) -> a -> Expr c
data (>=>) f g x :: Expr c
type instance Eval ((>=>) f g x) = Eval (g (Eval (f x)))

type (<$>) :: forall (a :: Type) (b :: Type). (a -> b) -> Expr a -> Expr b
data (<$>) f mx :: Expr b
type instance Eval (f <$> mx) = f (Eval mx)

type (<*>) :: forall (a :: Type) (b :: Type). Expr (a -> b) -> Expr a -> Expr b
data (<*>) mf mx :: Expr b
type instance Eval (mf <*> mx) = (Eval mf) (Eval mx)

type Join :: Expr (Expr a) -> Expr a
data Join mmx :: Expr a
type instance Eval (Join mmx) = Eval (Eval mmx)



type Free :: (Type -> Type) -> Type -> Type
data Free f a = Pure a | Free (f (Free f a))

-- data Exprr a where
--     BoolE :: Bool -> Exprr Bool

type SBool :: Bool -> Type
data SBool bool where
  STrue :: SBool True
  SFalse :: SBool False

-- data Con constraint where
--     CShow :: Con Show
--     CShowInt :: Con (Show Int)

type If :: forall (k :: Type). Bool -> k -> k -> k
type family If b true false where
    If 'True true _ = true
    If 'False _ false = false
-- type family If b true false :: k
-- type instance If 'True true _ = true
-- type instance If 'False _ false = false

type X = If @Nat 'True 2 (TypeError ('Text "This shouldn't happen"))

type TypeError' :: ErrorMessage -> Expr a
data TypeError' error :: Expr a

type instance Eval (TypeError' error) = TypeError error

type Z = Eval (If @(Expr Nat) 'True (Pure 2) (TypeError' ('Text "This shouldn't happen")))

type a ~> b = a -> b -> Type
type Apply :: forall (a :: Type) (b :: Type). a ~> b -> a -> b
-- type family Apply (f :: a ~> b) (x :: a) :: b
type family Apply f x :: b

-- type Eval_ :: forall (a :: Type). (() ~> a) -> a
-- type family Eval_ f :: a
-- type instance Eval_ f = Apply f '()
type Expr_ a = () ~> a
type Eval_ (e :: Expr_ a) = Apply e '()

-- a ~> b => a -> b -> Type => a -> Expr b
type Apply_ (f :: a -> Expr b) (x :: a) = Eval (f x)

-- type Apply_

-- type family Foo (x :: Type) (y :: Type) :: Type where
--   Foo Int Bool = Double
--   Foo Int Double = Maybe Char
--   Foo String (Maybe a) = Either Int a
--   Foo String (Maybe Int) = Maybe String  -- Warning: overlapped with the line above

-- type TYFUN :: Type
-- type TYFUN = Type -> Type -> Type
type TYFUN :: Type -> Type -> Type
data TYFUN :: Type -> Type -> Type
type TyFun a b = TYFUN a b -> Type

