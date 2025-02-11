-- {-# LANGUAGE ImpredicativeTypes #-}

module StInReaderT.Universal2 where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef

newtype RST r e s a = RST (ReaderT r (ExceptT e (ST s)) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader r
    )

unRST :: forall r e a. (forall s. RST r e s a) -> r -> (forall s. ST s (Either e a))
-- unRST (RST rst) r = runExceptT $ runReaderT rst r
unRST (RST rst) r = runExceptT . (`runReaderT` r) $ rst

-- runReaderT' :: forall r a. r -> (forall s. ReaderT r (ST s) a) -> (forall s. ST s a)
-- runReaderT' r m = runReaderT m r

runRST :: forall r e a. (forall s. RST r e s a) -> r -> Either e a
-- use ImpredicativeTypes extension
-- runRST rst r = runST . (`unRST` r) $ rst
-- ($) is impredicative by special compiler magic.
runRST rst r = runST $ unRST rst r

-- >>> test1
test1 :: Either Char Int
test1 = runRST test1' 2
  where
    test1' :: RST Int Char s Int
    test1' = do
      env <- ask
      pure (env + 1)

-- >>> test2
test2 :: Either Char Int
test2 = runRST test2' 2
  where
    test2' :: RST Int Char s Int
    test2' = do
      env <- ask
      ref <- RST (ReaderT (\_ -> ExceptT (Right <$> newSTRef @Int 1)))
      value <- RST (ReaderT (\_ -> ExceptT (Right <$> readSTRef ref)))
      pure (env + value)
