{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Concurrency.PoorMan where

import Concurrency.Class
import Concurrency.Msg
import Control.Monad (ap, liftM)
import Data.IORef qualified as IO
import Network.Socket qualified as Socket
import System.IO qualified as IO

data Action
  = Atomic (IO Action) -- an atomic computation, returning a new action
  | Fork Action Action -- create a new thread
  | Stop -- terminate this thread

writeAction :: String -> Action
writeAction = \case
  "" -> Stop
  c : cs -> Atomic $ do
    putChar c
    pure $ writeAction cs

prog :: Action
prog = Fork (writeAction "Hello\n") (writeAction "CIS 5520\n")

scheduled :: [Action] -> IO ()
scheduled = \case
  [] -> pure ()
  action' : actions ->
    case action' of
      Atomic io -> do
        action <- io
        scheduled (actions <> [action])
      Fork action1 action2 -> scheduled (actions <> [action2, action1])
      Stop -> scheduled actions

writeComputation :: String -> Action -> Action
writeComputation input k = case input of
  "" -> k
  c : cs -> Atomic $ do
    putChar c
    pure (writeComputation cs k)

prog3 :: Action
prog3 = writeComputation "Hello" (writeComputation " CIS 5520\n" Stop)

sequenceComputation ::
  (Action -> Action) ->
  (Action -> Action) ->
  (Action -> Action)
sequenceComputation = (.)

hello5520Computation :: Action -> Action
hello5520Computation =
  writeComputation "Hello" `sequenceComputation` writeComputation " CIS5520\n"

-- >>> scheduled [ hello5520Computation Stop ]
-- >>> scheduled [ Fork (hello5520Computation Stop) (hello5520Computation Stop) ]
-- >>> let bomb = writeComputation "bomb" bomb
-- scheduled [ bomb ]

readComputation :: (Char -> Action) -> Action
readComputation f = Atomic $ f <$> getChar

sequenceComp ::
  ((a -> Action) -> Action) -> -- last action takes an argument
  (a -> (b -> Action) -> Action) -> -- pass to another
  (b -> Action) ->
  Action
sequenceComp f g h = f (`g` h)

type CM a = (a -> Action) -> Action

sequenceCompM :: CM a -> (a -> CM b) -> CM b
sequenceCompM f g k = f (`g` k)

returnCompM :: a -> CM a
returnCompM x k = k x

newtype C a = MkC {runC :: (a -> Action) -> Action}

instance Functor C where
  fmap :: (a -> b) -> C a -> C b
  fmap f m = MkC (\k -> runC m (k . f))

instance Applicative C where
  pure :: a -> C a
  pure x = MkC (\k -> k x)
  (<*>) :: C (a -> b) -> C a -> C b
  (<*>) = ap

instance Monad C where
  (>>=) :: C a -> (a -> C b) -> C b
  m >>= f = MkC (\k -> runC m (\x -> runC (f x) k))

atomic :: IO a -> C a
atomic io = MkC (\k -> Atomic $ k <$> io)

-- Create a fork action with the given computation (stopping on completion)
-- and the the current continuation.
fork :: C () -> C ()
fork m = MkC (\k -> Fork (runC m (const Stop)) (k ()))

run :: C a -> IO ()
run m = scheduled [runC m (const Stop)]

instance Output C where
  output :: String -> C ()
  -- output = atomic . output
  output "" = atomic (output "")
  output (c : cs) = atomic (output [c]) >> output cs

infinite :: (Output m) => String -> m ()
infinite = (>>) <$> output <*> infinite

-- infinite msg = output msg >> infinite msg

example :: C ()
example = do
  output "It's raining..."
  fork (infinite "dog\n")
  infinite "cat\n"

instance Input C where
  input :: C (Maybe String)
  input = atomic input

ioLoop :: (Input m, Output m) => String -> m String
ioLoop s = do
  input >>= \case
    Just x -> pure $ "Thread " <> s <> ": " <> x
    Nothing -> do
      output s
      ioLoop s

example2 :: C ()
example2 = do
  fork $ ioLoop "a" >>= output
  ioLoop "b" >>= output

instance MsgMonad MailBox C where
  newMailBox :: C MailBox
  newMailBox = atomic newMailBox

  sendMsg :: MailBox -> Msg -> C ()
  sendMsg v msg = atomic (sendMsg v msg)

  checkMsg :: MailBox -> C (Maybe Msg)
  checkMsg v = atomic (checkMsg v)

-- simulation :: MailBox -> Integer -> C ()
-- simulation mailBox integer =
--     checkMsg mailBox >>= \case
--         Just Add -> do
--             output "Adding...\n"
--             simulation mailBox (integer + 1)
--         Just Reset -> do
--             output "Resetting...\n"
--             simulation mailBox 0
--         Just Print -> do
--             output ("Current value is " <> show integer <> "\n")
--             simulation mailBox integer
--         Just Quit -> do
--             output "Done\n"
--         Nothing -> simulation mailBox integer

-- interface :: MailBox -> C (Maybe String) -> C ()
-- interface mailBox getInput = loop where
--     loop = getInput >>= \case
--         Just "a" -> sendMsg mailBox Add >> loop
--         Just "r" -> sendMsg mailBox Reset >> loop
--         Just "p" -> sendMsg mailBox Print >> loop
--         Just "q" -> sendMsg mailBox Quit
--         Just s -> output ("Unknown command: " <> s <> "\n") >> loop
--         Nothing -> loop

example6 :: C ()
example6 = do
  mailBox <- newMailBox
  fork $ simulation mailBox 0
  -- interface mailBox input
  interface mailBox
