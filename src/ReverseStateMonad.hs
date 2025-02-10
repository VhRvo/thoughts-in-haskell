{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module ReverseStateMonad where

import Control.Monad.State hiding (state)
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

-- >> cumulative (Map.fromList [(1, "one"), (3, "three"), (2, "two")])
cumulative :: (Monoid w, Traversable t) => t w -> t w
cumulative t =
  evalState
    ( traverse
        ( \w -> do
            state <- get
            let new = state <> w
            put new
            pure new
        )
        t
    )
    mempty

testL :: Map Int Text
testL = cumulative (Map.fromList [(1, "one"), (3, "three"), (2, "two")])

newtype ReverseState s a
  = ReverseState
  { runReverseState :: s -> (a, s)
  }

instance Functor (ReverseState s) where
  fmap :: (a -> b) -> ReverseState s a -> ReverseState s b
  fmap f mx = ReverseState $ \state ->
    first f (runReverseState mx state)

instance Applicative (ReverseState s) where
  pure :: a -> ReverseState s a
  pure x = ReverseState (x,)
  (<*>) :: ReverseState s (a -> b) -> ReverseState s a -> ReverseState s b
  mf <*> mx = ReverseState $ \state ->
    let (f, past) = runReverseState mf future
        (x, future) = runReverseState mx state
     in (f x, past)

instance Monad (ReverseState s) where
  (>>=) :: ReverseState s a -> (a -> ReverseState s b) -> ReverseState s b
  mx >>= f = ReverseState $ \state ->
    let (x, past) = runReverseState mx future
        (result, future) = runReverseState (f x) state
     in (result, past)

evalReverseState :: ReverseState s a -> s -> a
evalReverseState m state = fst (runReverseState m state)

cumulativeR :: (Monoid w, Traversable t) => t w -> t w
cumulativeR t =
  evalReverseState
    ( traverse
        ( \w ->
            ReverseState $ \state -> let !r = w <> state in (r, r)
        )
        t
    )
    mempty

testR :: Map Int Text
testR = cumulativeR (Map.fromList [(1, "one"), (3, "three"), (2, "two")])
