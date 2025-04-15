{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -freduction-depth=0 -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module ZXCalculus where

import GHC.TypeLits
import Data.Proxy
import Data.Kind

import Unsafe.Coerce

type Phase = Int

data ZX :: Nat -> Nat -> Type where
  Empty     :: ZX 0 0
  Cup       :: ZX 0 2
  Cap       :: ZX 2 0
  Swap      :: ZX 2 2
  Wire      :: ZX 1 1
  Box       :: ZX 1 1
  X_Spider  :: Phase -> ZX n m
  Z_Spider  :: Phase -> ZX n m
  Stack     :: ZX n0 m0 -> ZX n1 m1 -> ZX (n0 + n1) (m0 + m1)
  -- StackN     :: n ZX n0 m0 -> ZX n1 m1 -> ZX (n0 + n1) (m0 + m1)
  Compose   :: ZX n m -> ZX m o -> ZX n o
  -- deriving (Show)


-- stack_n :: forall t m1 m2 n1 n2. (KnownNat t, KnownNat (Pred t), Mul t m1 ~ m2, Mul t n1 ~ n2) => Proxy t -> ZX n1 m1 -> ZX n2 m2
-- stack_n proxy zx
--   |  (Proxy @1) == 1 = zx
--   | otherwise = let t = natVal proxy in Stack zx (stack_n (Proxy @(Pred t)) zx)

-- Helper type class to handle base case
class StackN (t :: Nat) (n1 :: Nat) (m1 :: Nat) (n2 :: Nat) (m2 :: Nat) where
  stackN :: Proxy t -> ZX n1 m1 -> ZX n2 m2

-- Base case: t = 1
instance {-# OVERLAPPING #-} (n2 ~ n1, m2 ~ m1) => StackN 1 n1 m1 n2 m2 where
  stackN :: (n2 ~ n1, m2 ~ m1) => Proxy 1 -> ZX n1 m1 -> ZX n2 m2
  stackN _ zx = zx

-- Recursive case: t > 1
instance {-# OVERLAPPABLE #-} (KnownNat t,
          t ~ (1 + s),
          s ~ (t - 1),
          StackN s n1 m1 ns ms,
          n2 ~ (n1 + ns), m2 ~ (m1 + ms))
       => StackN t n1 m1 n2 m2 where
  stackN _ zx = Stack zx (stackN (Proxy @s) zx)

-- Wrapper function with nicer type
stack_n :: forall t n1 m1 n2 m2.
           (KnownNat t, StackN t n1 m1 n2 m2)
        => Proxy t -> ZX n1 m1 -> ZX n2 m2
stack_n = stackN @t

n_wires :: forall n. (KnownNat n, StackN n 1 1 n n) => Proxy n -> ZX n n
n_wires t = stackN @n t Wire

demo :: ZX 345 345
demo = n_wires (Proxy :: Proxy 345)

-- fusion_x_phase :: (KnownNat n, KnownNat m) => ZX n m -> ZX n m
-- fusion_x_phase (Compose (Stack ) ()) = X_Spider (p `mod` 2)
-- fusion_x_phase zx = zx

-- h :: ZX n m -> ZX n m
-- h (Compose pre zx2) =
--   case zx2 of
--     (Compose (Z_Spider a) post) -> if is_stack_box pre then X_Spider a else Compose pre zx2
--     (Compose (X_Spider a) post) -> if is_stack_box pre then Z_Spider a else Compose pre zx2
--     _ -> Compose pre zx2
-- h zx = zx

-- h_r :: forall n m. (KnownNat n, KnownNat m) => ZX n m -> ZX n m
-- h_r zx@(X_Spider a) =
--   let n1 = fromIntegral $ natVal (Proxy :: Proxy n) -- 提取 n
--       m1 = fromIntegral $ natVal (Proxy :: Proxy m) -- 提取 m
--   in
--     Compose (stack_n n Box) (Compose (Z_Spider a) (stack_n m Box))
-- h_r zx@(Z_Spider a) =
--   let n = fromIntegral $ natVal (Proxy :: Proxy n) -- 提取 n
--       m = fromIntegral $ natVal (Proxy :: Proxy m) -- 提取 m
--   in
--     Compose (stack_n n Box) (Compose (X_Spider a) (stack_n m Box))
-- h_r zx = zx

-- id_x_zero :: ZX 1 1 -> ZX 1 1
-- id_x_zero (X_Spider 0) = Wire
-- id_x_zero zx = zx

-- id_x_zero_r :: ZX 1 1 -> ZX 1 1
-- id_x_zero_r Wire = X_Spider 0

-- hh :: ZX 1 1 -> ZX 1 1
-- hh (Compose Box Box) = Wire
-- hh zx = zx

-- hh_r :: ZX 1 1 -> ZX 1 1
-- hh_r Wire = Compose Box Box
-- hh_r zx = zx

-- pi_copy :: ZX 1 n -> ZX 1 n
-- pi_copy (Compose pi zx) =
--   case (pi,zx) of
--     (pre@(X_Spider 180), Z_Spider a) -> Compose (Z_Spider (negate a)) (stack_n n pre)
--     (pre@(Z_Spider 180), X_Spider a) -> Compose (X_Spider (negate a)) (stack_n n pre)
--     _ -> Compose pi zx
-- pi_copy zx = zx

-- pi_copy_r :: ZX 1 n -> ZX 1 n
-- pi_copy_r zx@(Compose zx1 post) =
--   case (zx1,post) of
--     (Z_Spider a, post) -> if is_stacked post (is_pi_1_1 "X") then Compose post (Z_Spider (negate a)) else zx
--     (X_Spider a, post) -> if is_stacked post (is_pi_1_1 "Z") then Compose post (X_Spider (negate a)) else zx
--     _ -> zx
--   where
--     is_pi_1_1 "Z" (Z_Spider 180) = True
--     is_pi_1_1 "X" (X_Spider 180) = True
--     is_pi_1_1 _ _ = False
-- pi_copy_r zx = zx

-- o_copy :: ZX 0 n -> ZX 1 n
-- o_copy (Compose pi zx) =
--   case (pi,zx) of
--     (pre@(X_Spider 0), Z_Spider a) -> stack_n n pre
--     (pre@(Z_Spider 0), X_Spider a) -> stack_n n pre
--     _ -> Compose pi zx
-- o_copy zx = zx

-- o_copy_r :: Int -> ZX 1 n -> ZX 1 n
-- o_copy_r a zx@(Stack x@(X_Spider 0) z2) = if is_stacked zx (== x) then Compose z1 (Z_Spider a) else zx
-- o_copy_r a zx@(Stack z@(Z_Spider 0) z2) = if is_stacked zx (== z) then Compose z1 (X_Spider a) else zx
-- o_copy_r _ zx = zx
type family Pred (n :: Nat) :: Nat where
  Pred 0 = 0  -- 确保 0 的情况
  Pred n = n - 1

type family Mul (a :: Nat) (b :: Nat) :: Nat where
  Mul 0 b = 0
  Mul a 0 = 0
  Mul 1 b = b
  Mul a 1 = a
  Mul a b = a GHC.TypeLits.* b

cast_zx :: (n1 ~ n2, m1 ~ m2) => ZX n1 m1 -> ZX n2 m2
cast_zx = id




-- bi_algebra :: ZX 2 2 -> ZX 2 2
-- bi_algebra zx@(Compose zx1 (Compose zx2 zx3)) =
--   case (zx1, zx2, zx3) of
--     (X_Spider 0, Wire, Z_Spider 0) -> Compose (Stack (Z_Spider 0) (Z_Spider 0)) (Compose (Stack Wire (Stack Swap Wire)) (Stack (X_Spider 0) (X_Spider 0)))
--     (Z_Spider 0, Wire, X_Spider 0) -> Compose (Stack (X_Spider 0) (X_Spider 0)) (Compose (Stack Wire (Stack Swap Wire)) (Stack (Z_Spider 0) (Z_Spider 0)))
--     _ -> zx
-- bi_algebra zx = zx

-- bi_algebra_r :: ZX 2 2 -> ZX 2 2
-- bi_algebra_r zx@(Compose zx1 (Compose zx2 zx3)) =
--   case (zx1, zx2, zx3) of
--     (Stack (Z_Spider 0) (Z_Spider 0), Stack Wire (Stack Swap Wire), Stack (X_Spider 0) (X_Spider 0)) -> Compose (X_Spider 0) (Compose Wire (Z_Spider 0))
--     (Stack (X_Spider 0) (X_Spider 0), Stack Wire (Stack Swap Wire), Stack (Z_Spider 0) (Z_Spider 0)) -> Compose (Z_Spider 0) (Compose Wire (X_Spider 0))
--     _ -> zx
-- bi_algebra_r zx = zx

-- n_wires :: Int -> ZX n n
-- n_wires = flip stack_n Wire

-- n_box :: Int -> ZX n n
-- n_box = flip stack_n Box

is_stacked :: ZX n0 m0 -> (forall n1 m1. ZX n1 m1 -> Bool) -> Bool
is_stacked Empty _ = True
is_stacked (Stack zx1 zx2) p = p zx1 && is_stacked zx2 p
is_stacked _ _ = True

is_stack_box :: ZX n m -> Bool
is_stack_box = (`is_stacked` is_box)
  where
    is_box :: forall n1 m1. ZX n1 m1 -> Bool
    is_box Box = True
    is_box zx = False

-- is_stacked :: ZX n0 m0 -> (ZX n1 m1 -> Bool) -> Bool
-- is_stacked Empty _ = True
-- is_stacked (Stack zx1 zx2) p = p zx1 && is_stacked zx2 p
-- is_stacked _ _ = True

-- is_stack_box :: ZX n m -> Bool
-- is_stack_box = (`is_stacked` is_box)
--   where
--     is_box Box = True
--     is_box zx = False

-- stack_assoc :: ZX n m -> ZX n m
-- stack_assoc (Stack (Stack zx1 zx2) zx3) =  Stack zx1 (Stack zx2 zx3)
-- stack_assoc zx = zx

-- stack_assoc_r :: ZX n m -> ZX n m
-- stack_assoc_r (Stack zx1 (Stack zx2 zx3)) =  Stack (Stack zx1 zx2) zx3
-- stack_assoc_r zx = zx