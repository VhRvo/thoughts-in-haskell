module SAT.CStateT1 where

newtype CStateT s m a
  = CStateT {unCStateT :: forall r. (m a -> s -> r) -> s -> r}

-- instance Functor CState s where
--     fmap :: (a -> b) -> CState s a -> CState s b
--     fmap f (CState state) = CState $ \k s ->
--         state (\a s
--         )

-- newtype CState



