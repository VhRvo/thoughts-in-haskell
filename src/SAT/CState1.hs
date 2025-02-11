module SAT.CState1 where

newtype CState s a
  = CState {unCState :: forall r. (a -> s -> r) -> s -> r}

-- instance Functor CState s where
--     fmap :: (a -> b) -> CState s a -> CState s b
--     fmap f (CState state) = CState $ \k s ->
--         state (\a s
--         )

-- newtype CState
