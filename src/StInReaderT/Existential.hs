-- {-# LANGUAGE ImpredicativeTypes #-}

-- failed, don't know how to continue.

module StInReaderT.Existential where

import Control.Monad.ST
import Control.Monad.Reader

-- newtype RST r a = RST (ReaderT r (forall s. ST s) a)
--   deriving
--     ( Functor,
--       Applicative,
--       Monad,
--       MonadReader r
--     )
-- data RST r a = forall s. RST (ReaderT r (ST s) a)
--   deriving
--     ( Functor,
--       Applicative,
--       Monad,
--       MonadReader r
--     )

-- runRST :: forall r a. RST r a -> r -> a
-- runRST (RST rst) r = runST (runReaderT r rst)




