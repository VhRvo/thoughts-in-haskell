module SAT.CState where

import Control.Monad.Cont

newtype CState s a
  = CState (forall r. Cont (s -> r) a)

unCState :: CState s a -> forall r. (a -> s -> r) -> s -> r
-- unCState :: CState s a -> s -> forall r. ((a, s) -> r) -> r
-- unCState :: CState s a -> s -> (a, s)
unCState (CState cState) = runCont cState

-- instance CState s a
