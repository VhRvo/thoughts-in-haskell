module Concurrency.Class where

import Control.Monad.State qualified as S
import Control.Monad.Trans (MonadTrans (..))
import System.IO qualified as IO

class (Monad m) => Output m where
  output :: String -> m ()

class (Monad m) => Input m where
  input :: m (Maybe String) -- only return input if it is ready

instance Output IO where
  output :: String -> IO ()
  output = putStr

instance Input IO where
  input :: IO (Maybe String)
  input = do
    x <- IO.hReady IO.stdin
    if x
      then Just <$> getLine
      else pure Nothing

echo :: (Input m, Output m) => m ()
echo = do
  input >>= \case
    Just msg -> output msg >> output "\n"
    Nothing -> echo

instance (Output m) => Output (S.StateT s m) where
  output :: (Output m) => String -> S.StateT s m ()
  output str = lift (output str)

instance (Input m) => Input (S.StateT s m) where
  input :: (Input m) => S.StateT s m (Maybe String)
  input = lift input
