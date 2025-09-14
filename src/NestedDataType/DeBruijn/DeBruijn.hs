{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}

module NestedDataType.DeBruijn.DeBruijn where

import Data.Text (Text)
import qualified Data.Text as T

data Pair a = Pair a a
  deriving (Functor)

mapP :: (a -> b) -> Pair a -> Pair b
mapP f (Pair x y) = Pair (f x) (f y)

data Incr v
  = Zero
  | Succ v
  deriving (Functor)

mapI :: (a -> b) -> Incr a -> Incr b
mapI f = \case
  Zero -> Zero
  Succ v -> (Succ . f) v

data Term v
  = Var v
  | App (Pair (Term v))
  | Lam (Term (Incr v))
  deriving (Functor)

pattern PApp :: forall v. Term v -> Term v -> Term v
pattern PApp fun arg = App (Pair fun arg)

mapT :: forall a b. (a -> b) -> Term a -> Term b
mapT f = \case
  Var id -> (Var . f) id
  App pair -> (App . mapP (mapT f)) pair
  Lam body -> (Lam . mapT @(Incr a) @(Incr b) (mapI f)) body

-- foldTerm :: (t -> b) -> (Pair b -> b) -> (b -> b) -> Term t -> b
foldTerm ::
  forall n b.
  (forall a. a -> n a) ->
  (forall a. Pair (n a) -> n a) ->
  (forall a. n (Incr a) -> n a) ->
  Term b ->
  n b
-- foldTerm ::
--   forall b.
--   (forall a. a -> a) ->
--   (forall a. Pair a -> a) ->
--   (forall a. Incr a -> a) ->
--   Term b ->
--   b
foldTerm var _   _   (Var x) = var x
foldTerm var app lam (App pair) = (app . mapP (foldTerm var app lam)) pair
foldTerm var app lam (Lam body) = (lam . foldTerm var app lam) body

gfoldTerm ::
  forall m n b.
  (forall a. m a -> n a) ->
  (forall a. Pair (n a) -> n a) ->
  (forall a. n (Incr a) -> n a) ->
  (forall a. Incr (m a) -> m (Incr a)) ->
  Term (m b) ->
  n b
gfoldTerm var _   _   _ (Var x) = var x
gfoldTerm var app lam k (App pair) = (app . mapP (gfoldTerm var app lam k)) pair
gfoldTerm var app lam k (Lam body) = (lam . gfoldTerm var app lam k . mapT k) body

kfoldTerm ::
  forall a b.
  (a -> b) ->
  (Pair b -> b) ->
  (b -> b) ->
  (Incr a -> a) ->
  Term a ->
  b
kfoldTerm var _   _   _ (Var x) = var x
kfoldTerm var app lam k (App pair) = (app . mapP (kfoldTerm var app lam k)) pair
kfoldTerm var app lam k (Lam body) = (lam . kfoldTerm var app lam k . mapT k) body

showTerm :: Term Text -> Text
showTerm = kfoldTerm id showP ("Lam" <>) showI
  where
    showP (Pair x y) = "(" <> x <> " . " <> y <> ")"
    showI Zero = "0"
    showI (Succ x) = "S " <> x

showTC :: Term Char -> Text
showTC = showTerm . mapT wrap
  where
    wrap = T.singleton

term1 :: Term Char
term1 = Lam (PApp (Var Zero) (PApp (Var (Succ 'x')) (Var (Succ 'y'))))
-- term1 = Lam (App (Pair (Var Zero) (App (Pair (Var (Succ 'x')) (Var (Succ 'y'))))))

-- >>> showTC term1
-- "Lam(0 . (S x . S y))"

joinTerm :: Term (Term a) -> Term a
joinTerm = gfoldTerm id App Lam distTerm

distTerm :: Incr (Term a) -> Term (Incr a)
distTerm Zero        = Var Zero
distTerm (Succ term) = mapT Succ term

abstract :: (Eq a) => a -> Term a -> Term a
abstract x = Lam . mapT (match x)

match :: (Eq a) => a -> a -> Incr a
match x y
  | x == y    = Zero
  | otherwise = Succ y

apply :: Term a -> Term (Incr a) -> Term a
apply arg body = joinTerm (mapT (subst arg . mapI Var) body)

subst :: forall a. a -> Incr a -> a
subst x Zero     = x
subst _ (Succ y) = y

-- subst :: Term a -> Incr (Term a) -> Term a
-- subst x Zero = x
-- subst _ (Succ y) = y

-- t :: Term v
-- t = Lam (PApp (Var Zero) (Var (Succ Zero)))

-- >>> subst (Lam (PApp Zero (Succ Zero)))

