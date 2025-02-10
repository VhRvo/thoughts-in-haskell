{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}

module SmartView.ListT where

import Control.Applicative
import Control.Monad.Logic.Class (MonadLogic (..))
import Control.Monad.Trans
import Control.Monad (MonadPlus (..), ap, liftM, (>=>))
import Data.Bifunctor

data ListT m a
  = ListT (m (Maybe (a, ListT m a)))
  | ListT m a :+ ListT m a

data ListT m a
  = ListT (m (Maybe (a, ListT m a)))

-- viewListT :: (MonadPlus m) => ListT m a -> m (Maybe (a, ListT m a))
-- viewListT :: (Alternative m, Monad m) => ListT m a -> m (Maybe (a, ListT m a))
viewListT :: (Monad m) => ListT m a -> m (Maybe (a, ListT m a))
viewListT = \case
  ListT x -> x
  (m :+ n) :+ k -> viewListT (m :+ (n :+ k))
  m :+ n ->
    viewListT m >>= \case
      Nothing -> viewListT n
      Just (head, tail) -> pure (Just (head, tail <|> n))

-- instance (MonadPlus m) => Functor (ListT m) where
-- instance (Alternative m, Monad m) => Functor (ListT m) where
instance (Functor m) => Functor (ListT m) where
--   fmap :: (MonadPlus m) => (a -> b) -> ListT m a -> ListT m b
  fmap f m = ListT (fmap (fmap (bimap f (fmap f))) (viewListT m))

-- instance (MonadPlus m) => Applicative (ListT m) where
instance (Alternative m, Monad m) => Applicative (ListT m) where
--   pure :: (Monad m, Alternative m) => a -> ListT m a
  pure x = ListT (pure (Just (x, ListT (pure Nothing))))
--   (<*>) :: (MonadPlus m) => ListT m (a -> b) -> ListT m a -> ListT m b
  (<*>) = ap

-- instance (MonadPlus m) => Monad (ListT m) where
-- instance (Alternative m, Monad m) => Monad (ListT m) where
instance (Alternative m, Monad m) => Monad (ListT m) where
--   (>>=) :: (MonadPlus m) => ListT m a -> (a -> ListT m b) -> ListT m b
  m >>= f =
    ListT
      ( viewListT m >>= \case
          Nothing -> pure Nothing
          -- Just (head, tail) -> viewListT (f head) `mplus` viewListT (tail >>= f)
          Just (head, tail) -> viewListT (f head) <|> viewListT (tail >>= f)
      )

-- instance (Alternative m) => Alternative (ListT m) where
-- instance (MonadPlus m) => Alternative (ListT m) where
-- instance (Alternative m, Monad m) => Alternative (ListT m) where
instance (Monad m) => Alternative (ListT m) where
-- instance (Applicative m) => Alternative (ListT m) where
--   empty :: (MonadPlus m) => ListT m a
  empty = ListT (pure Nothing)
--   (<|>) :: (MonadPlus m) => ListT m a -> ListT m a -> ListT m a
  m <|> n = m :+ n

-- instance (MonadPlus m) => MonadPlus (ListT m) where
--   mzero :: (MonadPlus m) => ListT m a
--   mzero = empty
--   mplus :: (MonadPlus m) => ListT m a -> ListT m a -> ListT m a
--   mplus = (<|>)

-- instance (MonadPlus m) => MonadLogic (ListT m) where
--   msplit :: (MonadPlus m) => ListT m a -> ListT m (Maybe (a, ListT m a))
--   msplit x = lift (viewListT x)

instance MonadTrans ListT where
  lift :: (Monad m) => m a -> ListT m a
  lift m = ListT (m >>= (\x -> pure (Just (x, ListT (pure Nothing)))))

