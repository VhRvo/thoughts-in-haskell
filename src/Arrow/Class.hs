{-# LANGUAGE TupleSections #-}

module Arrow.Class where

import Control.Monad
import Prelude hiding ((.))

class Category arr where
  id :: a `arr` a
  (.) :: (b `arr` c) -> (a `arr` b) -> (a `arr` c)

class (Category arr) => Arrow arr where
  arr :: (b -> c) -> arr b c
  first :: arr b c -> arr (b, d) (c, d)
  second :: arr b c -> arr (d, b) (d, c)

class (Arrow arr) => ArrowApply arr where
  app :: arr (arr b c, b) c
  app2 :: b `arr` ((b `arr` c) `arr` c)
  -- app2 = arr (\x -> (app . arr (\f -> (f, x))))
  app2 = arr (\x -> app . arr (, x))

newtype Kleisli m a b
  = Kleisli {runKleisli :: a -> m b}

instance Category (->) where
  id :: a -> a
  id x = x
  (.) :: (b -> c) -> (a -> b) -> a -> c
  (.) f g x = f (g x)

instance Arrow (->) where
  arr :: (b -> c) -> b -> c
  arr f = f
  first :: (b -> c) -> (b, d) -> (c, d)
  first f (b, d) = (f b, d)
  second :: (b -> c) -> (d, b) -> (d, c)
  second f (d, b) = (d, f b)

instance ArrowApply (->) where
  app :: (b -> c, b) -> c
  app (f, b) = f b
  app2 :: b -> ((b -> c) -> c)
  app2 b f = f b

instance (Monad m) => Category (Kleisli m) where
  id :: Monad m => Kleisli m a a
  id = Kleisli pure
  (.) :: Monad m => Kleisli m b c -> Kleisli m a b -> Kleisli m a c
  (.) (Kleisli f) (Kleisli g) = Kleisli (g >=> f)

instance (Monad m) => Arrow (Kleisli m) where
  arr :: Monad m => (b -> c) -> Kleisli m b c
  arr f = Kleisli (pure . f)
--   arr f = Kleisli (\x -> pure (f x))
  first :: Monad m => Kleisli m b c -> Kleisli m (b, d) (c, d)
  first (Kleisli f) = Kleisli (\(b, d) -> (,d) <$> f b)
  second :: Monad m => Kleisli m b c -> Kleisli m (d, b) (d, c)
  second (Kleisli f) = Kleisli (\(d, b) -> (d,) <$> f b)

instance (Monad m) => ArrowApply (Kleisli m) where
  app :: Monad m => Kleisli m (Kleisli m b c, b) c
  app = Kleisli (\(Kleisli f, x) -> f x)
--   app2 = Kleisli (\x -> pure (Kleisli (\(Kleisli f) -> f x)))
  app2 :: Monad m => Kleisli m b (Kleisli m (Kleisli m b c) c)
  app2 = arr (\x -> Kleisli (\(Kleisli f) -> f x))
  -- app2 = arr (\x -> app . arr (\f -> (f, x)))

-- stage 1: hard-thinking: tools in my hands
-- stage 2: look at examples: (->), Kleisli
-- stage 3: eureka: what is possible
-- stage 4: reverse part: something is impossible (arr f :: (arr b c, b) `arr` c, because the desired function may have side-effect, but f :: (arr b c, b) -> c cannot support side-effect)
-- stage 4.5: conclude reverse is impossible: idApp is trivial, if you can use app2 to prove app, you can use trivial idApp to prove app

idApp :: ArrowApply arr => (b `arr` c) `arr` (b `arr` c)
idApp = arr (\f -> f)

idApp' :: ArrowApply arr => (b `arr` c) `arr` (b `arr` c)
idApp' = arr (\f -> app . arr (f ,))

idApp'' :: ArrowApply arr => (b `arr` c) `arr` (b `arr` c)
idApp'' = arr ((.) app . (arr . (,)))

f0 :: ArrowApply arr => b -> (b `arr` c) -> (b `arr` c, b)
f0 x f = (f, x)

f1 :: ArrowApply arr => b -> (b `arr` c) `arr` (b `arr` c, b)
f1 x = arr (, x)

f2 :: ArrowApply arr => b -> (b `arr` c) `arr` c
f2 x = app . arr (, x)

f3 :: ArrowApply arr => b `arr` ((b `arr` c) `arr` c)
f3  = arr (\x -> app . arr (, x))

g1 :: ArrowApply arr => (b `arr` c, b) `arr` b
g1 = arr (\(_, x) -> x)

g2 :: ArrowApply arr => ((b `arr` c) `arr` c) `arr` c
g2 = arr (\application -> undefined)





-- class Arrow (:->) where

