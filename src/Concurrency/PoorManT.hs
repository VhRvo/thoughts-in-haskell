{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Concurrency.PoorManT where

import Concurrency.Class
import Concurrency.Msg
import Control.Monad (ap, liftM, (>=>))
import qualified Control.Monad.State as S
import Control.Monad.State.Class
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.IORef as IO
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified System.IO as IO
import Test.HUnit (Test, runTestTT, (~?=))

data TraceIO a
  = Done a
  | Output String (TraceIO a)
  | Input (Maybe String -> TraceIO a)

instance Functor TraceIO where
  fmap :: (a -> b) -> TraceIO a -> TraceIO b
  fmap = liftM

instance Applicative TraceIO where
  pure :: a -> TraceIO a
  pure = Done
  (<*>) :: TraceIO (a -> b) -> TraceIO a -> TraceIO b
  (<*>) = ap

instance Monad TraceIO where
  (>>=) :: TraceIO a -> (a -> TraceIO b) -> TraceIO b
  mx >>= f = case mx of
    Done x -> f x
    Output output rest -> Output output (rest >>= f)
    Input g -> Input (g >=> f)

instance Output TraceIO where
  output :: String -> TraceIO ()
  output s = Output s (pure ())

instance Input TraceIO where
  input :: TraceIO (Maybe String)
  input = Input pure

echoTrace :: TraceIO ()
echoTrace =
  Input
    ( \case
        Just msg -> Output msg (Output "\n" (Done ()))
        Nothing -> echoTrace
    )

runTraceIO :: TraceIO () -> [Maybe String] -> [String]
runTraceIO = go
  where
    go (Done _) _ = []
    go (Output s restIo) inputs = s : go restIo inputs
    go (Input f) (input : inputs) = go (f input) inputs
    go (Input f) [] = go (f Nothing) []

testTraceEcho :: Test
testTraceEcho =
  runTraceIO echo [Nothing, Nothing, Just "hello"] ~?= ["hello", "\n"]

testTraceEcho2 :: Test
testTraceEcho2 =
  runTraceIO (echo >> echo) [Just "hello", Nothing, Just "CIS 5520"] ~?= ["hello", "\n", "CIS 5520", "\n"]

-- >>> runTestTT testTraceEcho2

data Action m
  = Atomic (m (Action m))
  | Fork (Action m) (Action m)
  | Stop

newtype C m a = MkC {runC :: (a -> Action m) -> Action m}

instance Functor (C m) where
  fmap :: (a -> b) -> C m a -> C m b
  fmap f mx = MkC (\k -> runC mx (k . f))

instance Applicative (C m) where
  pure :: a -> C m a
  pure x = MkC (\k -> k x)

  (<*>) :: C m (a -> b) -> C m a -> C m b
  mf <*> mx = MkC (\k -> runC mf (\f -> runC mx (k . f)))

instance Monad (C m) where
  return :: a -> C m a
  return = pure

  (>>=) :: C m a -> (a -> C m b) -> C m b
  mx >>= f = MkC (\k -> runC mx (\x -> runC (f x) k))

atomic :: (Monad m) => m a -> C m a
atomic mx = MkC (\k -> Atomic (k <$> mx))

instance MonadTrans C where
  lift :: (Monad m) => m a -> C m a
  lift = atomic

scheduled :: (Monad m) => [Action m] -> m ()
scheduled = \case
  [] -> pure ()
  action : actions -> case action of
    Atomic monadic -> do
      value <- monadic
      scheduled (actions <> [value])
    Fork action1 action2 -> scheduled (actions <> [action1, action2])
    Stop -> scheduled actions

run :: (Monad m) => C m a -> m ()
run m = scheduled [runC m $ const Stop]

fork :: C m () -> C m ()
fork m = MkC (\k -> Fork (runC m $ const Stop) (k ()))

instance (Input m) => Input (C m) where
  input :: C m (Maybe String)
  input = atomic input

instance (Output m) => Output (C m) where
  output :: String -> C m ()
  output msg = atomic (output msg)

example :: (Output m) => C m ()
example = do
  fork (output "Hello " >> output "5520")
  output "CIS"

runCTraceIO :: C TraceIO () -> [Maybe String] -> [String]
runCTraceIO = runTraceIO . run

testWrite :: Test
testWrite = runCTraceIO example [] ~?= ["Hello ", "CIS", "5520"]

-- >>> runCTraceIO example []
-- >>> runTestTT testWrite
-- ["Hello ","CIS","5520"]
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

instance (MsgMonad k m) => MsgMonad k (C m) where
  newMailBox :: (MsgMonad k m) => C m k
  newMailBox = lift newMailBox

  sendMsg :: (MsgMonad k m) => k -> Msg -> C m ()
  sendMsg k m = lift (sendMsg k m)

  checkMsg :: (MsgMonad k m) => k -> C m (Maybe Msg)
  checkMsg k = lift (checkMsg k)

example6 :: (Input m, Output m, MsgMonad k m) => C m ()
example6 = do
  mv <- newMailBox
  fork $ simulation mv 0
  interface mv

runCState :: (Monad m, m ~ TraceIO) => C (S.StateT Store TraceIO) () -> [Maybe String] -> [String]
runCState x inputs = x & run & (`S.evalStateT` Map.empty) & (`runTraceIO` inputs)

-- testCState :: Test
-- testCState =
--   runCState (example6 @_ @Int) [Just "a", Nothing, Nothing, Just "a", Just "p", Just "r", Just "p", Just "q", Just "x"]
--     ~?= ["Adding...\n", "Adding...\n", "Current value is 2\n", "Resetting...\n", "Current value is 0\n", "Done\n"]


-- >>> runTestTT testCState
