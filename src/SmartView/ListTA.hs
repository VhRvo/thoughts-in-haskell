{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartView.ListTA where

import Control.Applicative
import Control.Monad (MonadPlus (..), ap, liftM, (>=>))
import Control.Monad.Logic.Class (MonadLogic (..))
import Control.Monad.Trans
import Data.Bifunctor
import Prelude hiding (head, tail)

data ListT m a
  = ListT (m (Maybe (a, ListT m a)))
  | ListT m a :+ ListT m a

viewListT :: (Monad m) => ListT m a -> m (Maybe (a, ListT m a))
viewListT = \case
  ListT x -> x
  (m :+ n) :+ k -> viewListT (m :+ (n :+ k))
  m :+ n ->
    viewListT m >>= \case
      Nothing -> viewListT n
      Just (head, tail) -> pure (Just (head, tail :+ n))

instance (Monad m) => Functor (ListT m) where
  fmap :: (Monad m) => (a -> b) -> ListT m a -> ListT m b
  fmap f m = ListT (fmap (fmap (bimap f (fmap f))) (viewListT m))

instance (Monad m) => Applicative (ListT m) where
  pure :: (Monad m) => a -> ListT m a
  pure x = ListT (pure (Just (x, ListT (pure Nothing))))

  (<*>) :: (Monad m) => ListT m (a -> b) -> ListT m a -> ListT m b
  (<*>) = ap

instance (Monad m) => Monad (ListT m) where
  (>>=) :: (Monad m) => ListT m a -> (a -> ListT m b) -> ListT m b
  m >>= f =
    ListT
      ( viewListT m >>= \case
          Nothing -> pure Nothing
          Just (head, tail) -> viewListT (f head :+ (tail >>= f))
      )

instance (Monad m) => Alternative (ListT m) where
  empty :: (Monad m) => ListT m a
  empty = ListT (pure Nothing)

  (<|>) :: (Monad m) => ListT m a -> ListT m a -> ListT m a
  m <|> n = m :+ n

instance (MonadPlus m) => MonadPlus (ListT m) where
  mzero :: (MonadPlus m) => ListT m a
  mzero = empty

  mplus :: (MonadPlus m) => ListT m a -> ListT m a -> ListT m a
  mplus = (<|>)

instance (Monad m) => MonadLogic (ListT m) where
  msplit :: Monad m => ListT m a -> ListT m (Maybe (a, ListT m a))
  msplit x = lift (viewListT x)

instance MonadTrans ListT where
  lift :: (Monad m) => m a -> ListT m a
  lift m = ListT (m >>= (\x -> pure (Just (x, ListT (pure Nothing)))))
