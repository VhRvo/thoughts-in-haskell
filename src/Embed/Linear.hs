module Embed.Linear where

import Embed.Def

embed :: forall f a. (Functor f) => X f a -> Y f a
embed x = go x YZ
  where
    go :: forall b. (Functor f) => X f b -> (b -> Y f a) -> Y f a
    go (XZ x) cont = cont x
    go (XS x) cont = go @(f b) x (YS . fmap cont)

-- >>> embed demoX
-- YS [YS [YZ 1],YS [YZ 2,YZ 3],YS [YZ 4,YZ 5,YZ 6]]
