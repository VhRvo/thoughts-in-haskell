{-# LANGUAGE GADTs #-}

module Effectful.Demo where

import qualified Control.Monad.Except as T
import qualified Control.Monad.State as T
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

data HandlerState

data HandlerError

newtype Handler a
  = Handler (T.ExceptT HandlerError (T.StateT HandlerState IO) a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      T.MonadState HandlerState,
      T.MonadError HandlerError
    )

effToHandler :: Eff [Error HandlerError, State HandlerState, IOE] a -> Handler a
effToHandler m = do
  -- Retrieve the current state of the Handler.
  s <- T.get
  -- Run the Eff monad with effects mirroring the capabilities of @Handler@.
  (er, s') <- liftIO . runEff . runState s . runErrorNoCallStack @HandlerError $ m
  -- Update the state of the Handler and throw an error if appropriate.
  T.put s'
  either T.throwError pure er

data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic
