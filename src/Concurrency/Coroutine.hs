module Concurrency.Coroutine where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.State

-- import Data.Sequence

-- import Control.Monad.IO.Class

-- The CoroutineT monad is just ContT stacked with a StateT
-- containing the suspended coroutines.
newtype CoroutineT r m a
  = CoroutineT {runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a}
  deriving newtype (Functor, Applicative, Monad, MonadCont, MonadIO)

-- Used to manipulate the coroutine queue.
getCCs :: (Monad m) => CoroutineT r m [CoroutineT r m ()]
getCCs = CoroutineT $ lift get

putCCs :: (Monad m) => [CoroutineT r m ()] -> CoroutineT r m ()
putCCs = CoroutineT . lift . put

-- Pop and push coroutines to the queue.
dequeue :: (Monad m) => CoroutineT r m ()
dequeue = do
  getCCs >>= \case
    [] -> pure ()
    (process : processes) -> do
      putCCs processes
      process

queue :: (Monad m) => CoroutineT r m () -> CoroutineT r m ()
queue process = do
  ccs <- getCCs
  putCCs (ccs <> [process])

-- The interface.
yield :: (Monad m) => CoroutineT r m ()
yield = callCC $ \k -> do
  queue (k ())
  dequeue

fork :: (Monad m) => CoroutineT r m () -> CoroutineT r m ()
fork process = callCC $ \k -> do
  queue (k ())
  process
  dequeue

-- Exhaust passes control to suspended coroutines
-- repeatedly until there isn't any left.
exhaust :: (Monad m) => CoroutineT r m ()
exhaust = do
  exhausted <- null <$> getCCs
  unless exhausted $ do
    yield
    exhaust

runCoroutineT :: (Monad m) => CoroutineT r m r -> m r
runCoroutineT = (`evalStateT` []) . (`runContT` pure) . runCoroutineT' . (<* exhaust)

printOne :: (Show a) => a -> CoroutineT r IO ()
printOne n = do
  liftIO (print n)
  yield

example :: IO ()
example = runCoroutineT $ do
  fork $ replicateM_ 3 (printOne @Int 3)
  fork $ replicateM_ 4 (printOne @Int 4)
  replicateM_ 2 (printOne @Int 2)

-- >>> example
