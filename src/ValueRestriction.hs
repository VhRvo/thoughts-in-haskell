-- https://welltypedwit.ch/posts/value-restriction.html

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoMonoLocalBinds #-}

-- {-# LANGUAGE NamedFieldPuns #-}

module ValueRestriction where

import Data.Bifunctor
import Data.Functor.Identity
import Data.IORef
import Data.Semigroup
import GHC.Base

class (Monad m) => MonadGen m where
  generalize :: forall f. (forall a. m (f a)) -> m (forall a. f a)

instance MonadGen Identity where
  generalize :: forall f. (forall a. Identity (f a)) -> Identity (forall a. f a)
  generalize m =
    let -- x :: f a
        Identity x = m
     in -- x :: forall a. f a
        Identity x

blah :: Identity (Bool, Char)
blah =
  -- let -- doesn't need extra annotation.
  --     -- x :: forall a. Identity (Endo a)
  --     x = pure (Endo id)
  -- in (>>=) @_ @(forall a. Endo a) @(Bool, Char) (generalize x) (\f -> pure (appEndo f True, appEndo f 'a'))
  (>>=) @_ @(forall a. Endo a) @(Bool, Char)
    (generalize (pure (Endo id)))
    (\f -> pure (appEndo f True, appEndo f 'a'))

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f mx = State $ \state ->
    second f (runState mx state)

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (,x)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  mf <*> mx = State $ \state ->
    let (state', f) = runState mf state
        (state'', x) = runState mx state'
     in (state'', f x)

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  mx >>= f = State $ \state ->
    let (state', x) = runState mx state
        (result, state'') = runState (f x) state'
     in (result, state'')

instance MonadGen (State s) where
  generalize :: forall f. (forall a. State s (f a)) -> State s (forall a. f a)
  -- succeeds
  generalize m' =
    let -- m :: s -> (s, f a)
        State m = m'
     in -- m :: forall a. s -> (s, f a)
        State $ \state ->
          -- only `let` play well with impredicate types, `case` and `runState` are bad
          -- Conclusion: So it is very hard to deal sum type with impredicate type?
          let -- f :: f a
              (state', f) = m state
           in -- f :: forall a. f a
              (,) state' f

-- succeeds
--   generalize m' = State $ \state ->
--     -- only `let` play well with impredicate types, `case` and `runState` are bad
--     let -- m :: s -> (s, f a)
--         State m = m'
--         (state', f) = m state
--      in --  (,) @_ @(forall a. f a) state' f
--         (,) state' f
-- fails
--   generalize m' = State $ \state ->
--      case m' of
--         -- m :: s -> (s, f Any)
--         State m ->
--             let -- m :: s -> (s, f a)
--                 (state', f) = m state
--             in --  (,) @_ @(forall a. f a) state' f
--                 (,) state' f
-- fails
--   generalize m' = case m' of
--     -- m :: s -> (s, f Any)
--     State m ->
--       State $ \state ->
--         -- only `let` play well with impredicate types, `case` and `runState` are bad
--         let -- m :: s -> (s, f a)
--             (state', f) = m state
--         in --  (,) @_ @(forall a. f a) state' f
--             (,) state' f
-- fails
--   generalize (State m) = State $ \state ->
--     let (state', f) = m state
--     in (,) state' f
-- fails
--   generalize m' = State $ \state -> case m' of
--     -- m :: s -> (s, f Any)
--     State m ->
--       let (state', f) = m state
--       in (,) state' f

data BoxedState s = BoxedState {state :: State# s}

liftState :: (# State# s, b #) -> (BoxedState s, b)
liftState (# s, b #) = (BoxedState s, b)

instance MonadGen IO where
  generalize :: forall f. (forall a. IO (f a)) -> IO (forall a. f a)
  generalize m' =
    IO $ \state ->
      -- state :: State# RealWorld
      let -- m :: State# RealWorld -> (# State# RealWorld, f a #)
          IO m = m'
          -- m :: forall a. State# RealWorld -> (# State# RealWorld, f a #)
          (boxedState, result) = liftState (m state)
          !(BoxedState state') = boxedState
       in (# state', result #)

newtype MaybeRef a = MaybeRef {unMaybeRef :: IORef (Maybe a)}

newMaybeRef :: IO (MaybeRef a)
newMaybeRef = MaybeRef <$> newIORef Nothing

unsafeCoerceIO :: a -> IO b
unsafeCoerceIO v =
  (>>=)
    (generalize (MaybeRef <$> newIORef Nothing))
    ( \ref' ->
        let -- ref :: IORef (Maybe a)
            MaybeRef ref = ref'
         in -- ref :: forall {a}. IORef (Maybe a)
            (>>=)
              (writeIORef ref (Just v))
              ( \() ->
                  (>>=)
                    (readIORef ref)
                    ( \case
                        Just result -> pure result
                        Nothing -> error "unreachable"
                    )
              )
    )

-- Program in do-notation fails.
-- blah :: Identity (Bool, Char)
-- blah = do
--     -- Endo id :: Endo a
--     -- pure (Endo id) :: forall a. Identity (Endo a)
--     let x :: forall a. Identity (Endo a)
--         x = pure (Endo id)
--     f :: forall a. Endo a <- generalize (pure (Endo id))
--     -- f :: forall a. Endo a <- generalize @Identity @Endo (pure (Endo id))
--     -- f :: forall a. Endo a <- generalize @Identity @Endo x
--     -- f :: forall a. Endo a <- generalize x
--     pure (appEndo f True, appEndo f 'a')
