{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Concurrency.Msg where

import Concurrency.Class
import Control.Monad.State (MonadState (..))
import Control.Monad.State.Strict qualified as S
import Control.Monad.Trans (MonadTrans (..))
import Data.IORef qualified as IO
import Data.Map.Strict qualified as Map

data Msg
  = Add
  | Reset
  | Print
  | Quit

class (Monad m) => MsgMonad b m | m -> b where
  newMailBox :: m b
  sendMsg :: b -> Msg -> m ()
  checkMsg :: b -> m (Maybe Msg)

type MailBox = IO.IORef (Maybe Msg)

instance MsgMonad MailBox IO where
  newMailBox :: IO MailBox
  newMailBox = IO.newIORef Nothing

  sendMsg :: MailBox -> Msg -> IO ()
  sendMsg v a = IO.writeIORef v (Just a)

  checkMsg :: MailBox -> IO (Maybe Msg)
  checkMsg v =
    IO.readIORef v >>= \case
      Just y -> IO.writeIORef v Nothing >> pure (Just y)
      Nothing -> pure Nothing

simulation :: (Output m, MsgMonad k m) => k -> Int -> m ()
simulation mv = loop
  where
    loop integer = do
      checkMsg mv >>= \case
        Just Add -> output "Adding...\n" >> loop (integer + 1)
        Just Reset -> output "Resetting...\n" >> loop 0
        Just Print -> output ("Current value is " <> show integer <> "\n") >> loop integer
        Just Quit -> output "Done\n"
        Nothing -> loop integer

interface :: (MsgMonad k m, Input m, Output m) => k -> m ()
interface mv = loop
  where
    loop = do
      input >>= \case
        Just key -> case key of
          "a" -> sendMsg mv Add >> loop
          "r" -> sendMsg mv Reset >> loop
          "p" -> sendMsg mv Print >> loop
          "q" -> sendMsg mv Quit
          s -> output ("Unknown command: " <> s <> "\n") >> loop
        Nothing -> loop

type Store = Map.Map Int (Maybe Msg)

instance (Monad m) => MsgMonad Int (S.StateT Store m) where
  newMailBox :: (Monad m) => S.StateT Store m Int
  newMailBox = do
    state <- get
    let indexes = fst <$> Map.toList state
    let newIndex = if null indexes then 0 else 1 + maximum indexes
    put (Map.insert newIndex Nothing state)
    pure newIndex
  sendMsg :: (Monad m) => Int -> Msg -> S.StateT Store m ()
  sendMsg k msg = do
    state <- get
    put (Map.insert k (Just msg) state)
  checkMsg :: (Monad m) => Int -> S.StateT Store m (Maybe Msg)
  checkMsg k = do
    state <- get
    case Map.lookup k state of
      Just value -> do
        case value of
          Just value' -> do
            put (Map.insert k Nothing state)
            pure (Just value')
          Nothing -> pure Nothing
      Nothing ->
        pure Nothing
