{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartView.ListT where

import Control.Applicative
import Control.Monad (MonadPlus (..), ap, liftM, (>=>))
import Control.Monad.Logic.Class (MonadLogic (..))
import Control.Monad.Trans
import Data.Bifunctor
import Prelude hiding (head, tail)

data ListT m a
  = ListT (m (Maybe (a, ListT m a)))
  | ListT m a :+ ListT m a

newtype ListTView m a
  = ListTView {unListTView :: m (Maybe (a, ListT m a))}

-- type ListTView m a = m (Maybe (a, ListT m a))
-- unListTView = id

-- instance Alternative (ListTView m) where
--   empty :: ListTView m a
--   empty = ListTView (pure Nothing)
--   (<|>) :: ListTView m a -> ListTView m a -> ListTView m a
--   ListTView left <|> ListTView right = ListTView ((<|>) <$> left <*> right)

-- viewListT :: (MonadPlus m) => ListT m a -> m (Maybe (a, ListT m a))
-- viewListT :: (Alternative m, Monad m) => ListT m a -> m (Maybe (a, ListT m a))
viewListT :: (Monad m) => ListT m a -> ListTView m a
viewListT = \case
  ListT x -> ListTView x
  (m :+ n) :+ k -> viewListT (m :+ (n :+ k))
  m :+ n ->
    ListTView
      ( unListTView (viewListT m) >>= \case
          Nothing -> unListTView (viewListT n)
          Just (head, tail) -> pure (Just (head, tail :+ n))
      )

-- instance (MonadPlus m) => Functor (ListT m) where
-- instance (Alternative m, Monad m) => Functor (ListT m) where
instance (Monad m) => Functor (ListT m) where
  fmap :: (Monad m) => (a -> b) -> ListT m a -> ListT m b
  fmap f m = ListT (fmap (fmap (bimap f (fmap f))) (unListTView (viewListT m)))

-- instance (MonadPlus m) => Applicative (ListT m) where
instance (Monad m) => Applicative (ListT m) where
  pure :: (Monad m) => a -> ListT m a
  pure x = ListT (pure (Just (x, ListT (pure Nothing))))

  (<*>) :: (Monad m) => ListT m (a -> b) -> ListT m a -> ListT m b
  (<*>) = ap

-- instance (MonadPlus m) => Monad (ListT m) where
-- instance (Alternative m, Monad m) => Monad (ListT m) where
instance (Monad m) => Monad (ListT m) where
  --   (>>=) :: (MonadPlus m) => ListT m a -> (a -> ListT m b) -> ListT m b
  (>>=) :: (Monad m) => ListT m a -> (a -> ListT m b) -> ListT m b
  m >>= f =
    ListT
      ( unListTView (viewListT m) >>= \case
          Nothing -> pure Nothing
          -- Just (head, tail) -> viewListT (f head) `mplus` viewListT (tail >>= f)
          -- Just (head, tail) -> viewListT (f head) :+ ListT (unListTView (viewListT (tail >>= f)))
          Just (head, tail) -> unListTView (viewListT (f head :+ (tail >>= f)))
      )

-- instance (Alternative m) => Alternative (ListT m) where
-- instance (MonadPlus m) => Alternative (ListT m) where
-- instance (Alternative m, Monad m) => Alternative (ListT m) where
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
  msplit x = lift (unListTView (viewListT x))

instance MonadTrans ListT where
  lift :: (Monad m) => m a -> ListT m a
  lift m = ListT (m >>= (\x -> pure (Just (x, ListT (pure Nothing)))))
