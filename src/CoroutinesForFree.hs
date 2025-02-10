-- {-# LANGUAGE StrictData #-}

module CoroutinesForFree where

import Data.Functor.Identity
import Control.Monad.Free

data Trampoline' a
    = Return' a
    | Bounce' (Trampoline' a)

runTrampoline' :: Trampoline' a -> a
runTrampoline' = \case
    Return' a -> a
    Bounce' t -> runTrampoline' t

type Trampoline = Free Identity

bounce :: Trampoline a -> Trampoline a
bounce = Free . Identity

runTrampoline :: Trampoline a -> a
runTrampoline = runIdentity . retract

-- data Producer a
--     = Done
--     | Yield a (Producer a)
type Producer a = Free ((,) a) ()

yield :: a -> Producer a
yield a = Free (a, pure ())

consumeProducer :: Producer a -> [a]
consumeProducer = \case
    Pure () -> []
    Free (a, continuation) -> a : consumeProducer continuation

producerExample = consumeProducer $ do
    yield 1
    yield 2
    yield 3

-- data Consumer a b
--     = Result b
--     | Await (a -> Consumer a b)
type Consumer a = Free ((->) a)

await :: Consumer a a
await = Free pure
-- await = Free Pure

pipe :: Producer a -> Consumer a r -> Maybe r
pipe _ (Pure result) = Just result
pipe (Free (a, c)) (Free f) = pipe c $ f a
pipe _ _ = Nothing
