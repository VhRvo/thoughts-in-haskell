{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE Strict #-}

module Trampoline where

-- import Control.Monad (join)
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor

newtype Trampoline m a = Trampoline
  { bounce :: m (Either (Trampoline m a) a)
  }

instance (Functor m) => Functor (Trampoline m) where
  fmap :: forall a b. (a -> b) -> Trampoline m a -> Trampoline m b
  fmap f (Trampoline trampoline) = Trampoline (fmap (bimap (fmap f) f) trampoline)

-- instance Applicative m => Applicative (Trampoline m) where
instance (Monad m) => Applicative (Trampoline m) where
  pure :: a -> Trampoline m a
  pure = Trampoline . pure . Right
  (<*>) :: forall a b. Trampoline m (a -> b) -> Trampoline m a -> Trampoline m b
  -- (Trampoline f) <*> tx = Trampoline (fmap (Left . either (<*> tx) (<$> tx)) f)
  (Trampoline mf) <*> tx = Trampoline (mf >>= either (pure . Left . (<*> tx)) (bounce . (<$> tx)))

-- The Monad give m the ability to strip off layers, whereas in Functor you can just add a new layer.
-- instance Applicative m => Monad (Trampoline m) where
--     (>>=) :: forall a b. Trampoline m a -> (a -> Trampoline m b) -> Trampoline m b
--     (Trampoline tx) >>= f = Trampoline (fmap (Left . either (>>= f) f) tx)
-- (Trampoline tx) >>= f = Trampoline (fmap (either (Left . (>>= f)) (join . bounce . f)) tx)

instance (Monad m) => Monad (Trampoline m) where
  (>>=) :: forall a b. Trampoline m a -> (a -> Trampoline m b) -> Trampoline m b
  -- (Trampoline tx) >>= f = Trampoline (fmap (Left . either (>>= f) f) tx)
  (Trampoline tx) >>= f = Trampoline (tx >>= either (pure . Left . (>>= f)) (bounce . f))

instance MonadTrans Trampoline where
  lift :: forall m a. (Functor m) => m a -> Trampoline m a
  lift = Trampoline . fmap Right

pause :: (Monad m) => Trampoline m ()
-- pause = Trampoline . pure . Right $ ()
pause =
  Trampoline . pure . Left . pure $ ()

run :: (Monad m) => Trampoline m r -> m r
run (Trampoline t) =
  t >>= either run pure

mZipWith :: forall m a b c. (Monad m) => (a -> b -> c) -> Trampoline m a -> Trampoline m b -> Trampoline m c
mZipWith f t1 t2 = Trampoline (bind <$> bounce t1 <*> bounce t2)
  where
    bind :: Either (Trampoline m a) a -> Either (Trampoline m b) b -> Either (Trampoline m c) c
    bind (Left a) (Left b) = Left (mZipWith f a b)
    bind (Left a) (Right b) = Left (mZipWith f a (pure b))
    bind (Right a) (Left b) = Left (mZipWith f (pure a) b)
    bind (Right a) (Right b) = Right (f a b)

interleave :: forall m a. (Monad m) => [Trampoline m a] -> Trampoline m [a]
interleave = foldr (mZipWith (:)) (pure [])

hello :: Trampoline IO ()
hello = do
  lift (putStr "Hello, ")
  lift (putStr "Hello, ")
  -- pause
  Trampoline (pure $ Left $ lift $ putStr "Paused ")
  lift (putStrLn "World!")

wonderful :: IO ()
wonderful = do
  continuation <- bounce hello
  putStr "Wonderful "
  case continuation of
    Left trampoline -> run trampoline
    Right () -> pure ()

interleaved :: IO [()]
interleaved = run $ interleave [hello, hello, hello]
