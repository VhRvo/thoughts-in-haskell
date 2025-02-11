-- {-# LANGUAGE ImpredicativeTypes #-}

module StInReaderT.Universal where

import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef

newtype RST r s a = RST (ReaderT r (ST s) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader r
    )

unRST :: forall r a. (forall s. RST r s a) -> (forall s. ReaderT r (ST s) a)
unRST (RST rst) = rst

runReaderT' :: forall r a. r -> (forall s. ReaderT r (ST s) a) -> (forall s. ST s a)
runReaderT' r m = runReaderT m r

runRST :: forall r a. (forall s. RST r s a) -> r -> a
-- ($) is impredicative by special compiler magic.
runRST rst r = runST $ runReaderT' r $ unRST rst

-- use ImpredicativeTypes extension
-- runRST rst r = runST . runReaderT' r . unRST $ rst

-- >>> test1
test1 :: Int
test1 = runRST test1' 2
  where
    test1' :: RST Int s Int
    test1' = do
      env <- ask
      pure (env + 1)

-- >>> test2
test2 :: Int
test2 = runRST test2' 2
  where
    test2' :: RST Int s Int
    test2' = do
      env <- ask
      ref <- RST (ReaderT (\_ -> newSTRef @Int 1))
      value <- RST (ReaderT (\_ -> readSTRef ref))
      pure (env + value)
