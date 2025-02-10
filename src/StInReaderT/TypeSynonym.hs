module StInReaderT.TypeSynonym where

import Control.Monad.Reader
import Control.Monad.ST

type RST r s = ReaderT r (ST s)

runRST :: forall r a. (forall s. RST r s a) -> r -> a
runRST action r = runST $ runReaderT action r
