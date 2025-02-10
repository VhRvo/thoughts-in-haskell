{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE TypeAbstractions #-}
module Cont where
import Control.Monad.Cont hiding (cont, Cont, runCont)
-- import Control.Monad (when)

newtype Single a = Single { runSingle :: forall r. (a -> r) -> r }
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
newtype Single' a = Single' { runSingle' :: forall r. Cont r a }

toSingle :: forall a. a -> Single a
toSingle x = Single (\f -> f x)

fromSingle :: forall a. Single a -> a
fromSingle (Single f) = f id

instance Functor Single where
    fmap :: (a -> b) -> Single a -> Single b
    fmap f single = Single (\k -> runSingle single (k . f))

instance Applicative Single where
    pure :: a -> Single a
    pure x = Single (\k -> k x)

    (<*>) :: Single (a -> b) -> Single a -> Single b
    (<*>) f g = Single (\k -> runSingle f (\f' -> runSingle g (k . f')))

instance Monad Single where
    (>>=) :: Single a -> (a -> Single b) -> Single b
    mx >>= f = Single (\k -> runSingle mx (\x -> runSingle (f x) k))

-- instance MonadCont Single where
--     callCC :: ((a -> Single b) -> Single a) -> Single a
--     callCC f = Single (\k -> runSingle (f (\x -> Single (\_ -> k x)) ) k)
--                                                                ^^^


toCont :: forall a r. a -> Cont r a
toCont x = Cont (\f -> f x)

fromCont :: forall a. (forall r. Cont r a) -> a
fromCont cont = runCont @_ @a cont id

-- Couldn't match expected type ‘a’ with actual type ‘r’
-- fromCont :: forall r a. Cont r a -> a
-- fromCont (Cont f) = f id


instance Functor (Cont r) where
    fmap :: (a -> b) -> Cont r a -> Cont r b
    fmap f cont = Cont (\k -> runCont cont (k . f))

instance Applicative (Cont r) where
    pure :: a -> Cont r a
    pure x = Cont (\k -> k x)

    (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
    (<*>) f g = Cont (\k -> runCont f (\f' -> runCont g (k . f')))

instance Monad (Cont r) where
    (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
    mx >>= f = Cont (\k -> runCont mx (\x -> runCont (f x) k))

-- instance Monad Maybe where
--     (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--     mx >>= f = case mx of
--         Just x -> f x
--         Nothing -> Nothing

-- instance Monad (State s) where
--     (>>=) :: State s a -> (a -> State s b) -> State s b
--     mx >>= f = State (\s ->
--         let (a, s') = runState mx s
--          in runState (f a) s')

escape :: (a -> r) -> a -> Cont r b
escape k x = Cont (\_ -> k x)

instance MonadCont (Cont r) where
    callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
    callCC f = Cont (\k -> runCont (f (escape k)) k)


-- >>> 1 + 2
-- 3


-- quux :: Cont r Int
-- quux = callCC $ \k -> do
--     let n = 5
--     -- when True (k n)
--     k 25

