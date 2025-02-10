module Traversable.MapAccum where

-- import Data.Traversable ()
-- import Data.Traversable hiding (mapAccumL, mapAccumR)
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy

mapAccumL :: Traversable t => (a -> s -> (b, s)) -> s -> t a -> (t b, s)
mapAccumL combine initial traversable = Strict.runState (traverse (Strict.state . combine) traversable) initial

-- mapAccumR :: Traversable t => (a -> s -> (b, s))
-- -- wrong implementation: Lazy.runState (traverse (Lazy.state . combine) traversable) initial-> s -> t a -> (t b, s)
-- mapAccumR combine initial traversable = undefined

-- mapAccumL' :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
-- mapAccumL' combine initial traversable = undefined

-- mapAccumR' :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
-- mapAccumR' combine initial traversable = undefined
