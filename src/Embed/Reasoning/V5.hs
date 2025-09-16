{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
module Embed.Reasoning.V5 where

import Embed.Def

embed :: forall f a. (Functor f) => X f a -> Y f a
embed             (XZ x)    =                                 YZ   $ x
embed         (XS (XZ x))   =                       YS . fmap YZ   $ x
embed     (XS (XS (XZ x)))  =            YS . fmap (YS . fmap YZ)  $ x
embed (XS (XS (XS (XZ x)))) = YS . fmap (YS . fmap (YS . fmap YZ)) $ x
embed (XS (XS (XS (XS x)))) = collapse . collapse . collapse . collapse . embed $ x


collapse :: forall f a. (Functor f) => Y f (f a) -> Y f a
collapse (YZ x) = YS . fmap YZ       $ x
collapse (YS x) = YS . fmap collapse $ x
