{-# HLINT ignore "Avoid lambda" #-}

module PHOAS.PStream where

import Data.Function

data PStream a v
  = Var v
  | Mu (v -> PStream a v)
  | Cons a (PStream a v)

newtype Stream a = Hide {reveal :: forall v. PStream a v}

junk :: Stream a
junk = Hide (Mu Var)

s1 :: Stream Int
s1 = Hide (Cons 1 (Mu (\v -> Cons 2 (Var v))))

s2 :: Stream Int
s2 = Hide (Mu (\v -> Cons 1 (Cons 2 (Var v))))

elems :: Stream a -> [a]
elems stream = elems' (reveal stream)
  where
    elems' :: PStream a [a] -> [a]
    elems' stream = case stream of
      Var a -> a
      Mu f -> elems' (f [])
      Cons a as -> a : elems' as

--- >>> s1
-- No instance for `Show (Stream Int)'
--   arising from a use of `evalPrint'
-- There are instances for similar types:
--   instance [safe] Show a => Show (Stream a)
--     -- Defined in `Text.Pretty.Simple.Internal.Printer'
-- In a stmt of an interactive GHCi command: evalPrint it_a14al

foldStream :: forall a b. (a -> b -> b) -> b -> Stream a -> b
foldStream combine initial stream = foldStream' (reveal stream)
  where
    foldStream' :: PStream a b -> b
    foldStream' stream = case stream of
      Var a -> a
      Mu f -> foldStream' (f initial)
      Cons a as -> combine a (foldStream' as)

foldStream' :: forall a b. (a -> b -> b) -> b -> Stream a -> b
foldStream' combine initial stream = fold (reveal stream)
  where
    fold :: PStream a () -> b
    fold stream = case stream of
      Var () -> initial
      Mu f -> fold (f ())
      Cons a as -> combine a (fold as)

cfoldStream :: forall a b. (a -> b -> b) -> Stream a -> b
cfoldStream combine = cfoldStream' . reveal
  where
    cfoldStream' :: PStream a b -> b
    cfoldStream' stream = case stream of
      Var a -> a
      Mu f -> fix (cfoldStream' . f)
      Cons a as -> combine a (cfoldStream' as)

smap :: forall a b. (a -> b) -> Stream a -> Stream b
-- Couldn't match expected type: forall v. PStream b v
--   with actual type: PStream b v0
-- smap f = Hide . smap' . reveal
smap f stream = Hide (smap' (reveal stream))
  where
    smap' :: forall v. PStream a v -> PStream b v
    smap' stream = case stream of
      Var a -> Var a
      Mu g -> Mu (\v -> smap' (g v))
      Cons a as -> Cons (f a) (smap' as)

instance (Eq a) => Eq (Stream a) where
  (==) :: Stream a -> Stream a -> Bool
  stream1 == stream2 = eq' 0 (reveal stream1) (reveal stream2)
    where
      eq' :: forall a. (Eq a) => Int -> PStream a Int -> PStream a Int -> Bool
      eq' _ (Var x) (Var y) = x == y
      eq' n (Mu f) (Mu g) = eq' (n + 1) (f n) (g n)
      eq' n (Cons x xs) (Cons y ys) = x == y && eq' n xs ys
      eq' _ _ _ = False

returnPStream :: forall v a. v -> PStream a v
returnPStream = Var

joinPStream :: forall a v. PStream a (PStream a v) -> PStream a v
joinPStream stream = case stream of
  Var v -> v
  Mu f -> Mu (joinPStream . f . Var)
  Cons a as -> Cons a (joinPStream as)

unrollStream :: Stream a -> Stream a
unrollStream stream = Hide (joinPStream (unroll (reveal stream)))
  where
    unroll :: PStream a (PStream a v) -> PStream a (PStream a v)
    unroll (Mu g) = g (joinPStream (Mu g))
    unroll (Cons x xs) = Cons x (unroll xs)
    unroll (Var _) = error ""

-- tailStream :: Stream a -> Stream a
-- tailStream stream = Hide (join ())

tailStream :: forall a. Stream a -> Stream a
tailStream stream = Hide (joinPStream (tail (reveal stream)))
  where
    tail :: forall v. PStream a (PStream a v) -> PStream a (PStream a v)
    tail (Var v) = Var v
    tail (Cons _ xs) = xs
    tail (Mu g) =
      Mu
        ( \x ->
            let phead (Mu g) = phead (g x)
                phead (Cons y _) = y
                phead (Var _) = error ""
             in tail (g (Cons (phead (g x)) x))
        )
