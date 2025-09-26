{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
module Embed.Reasoning.V4 where

import Embed.Def

embed :: forall f a. (Functor f) => X f a -> Y f a
embed (XZ x) = YZ $ x
embed (XS (XZ x)) = YS . fmap YZ $ x
embed (XS (XS (XZ x))) = YS . fmap (YS . fmap YZ) $ x
-- embed (XS (XS (XS x))) = collapse . collapse . collapse . embed $ x
-- embed (XS (XS (XS (XZ x)))) = collapse . collapse . collapse . embed . XZ $ x
-- embed (XS (XS (XS (XZ x)))) = collapse . collapse . collapse . YZ $ x
-- embed (XS (XS (XS (XZ x)))) = collapse . collapse . YS . fmap YZ $ x
-- embed (XS (XS (XS (XZ x)))) = collapse . YS . fmap collapse . fmap YZ $ x
-- embed (XS (XS (XS (XZ x)))) = YS . fmap collapse . fmap collapse . fmap YZ $ x
-- embed (XS (XS (XS (XZ x)))) = YS . fmap collapse . fmap (collapse . YZ) $ x
-- embed (XS (XS (XS (XZ x)))) = YS . fmap collapse . fmap (YS . fmap YZ) $ x
-- embed (XS (XS (XS (XZ x)))) = YS . fmap (collapse . YS . fmap YZ) $ x
-- embed (XS (XS (XS (XZ x)))) = YS . fmap (YS . fmap collapse . fmap YZ) $ x
-- embed (XS (XS (XS (XZ x)))) = YS . fmap (YS . fmap (collapse . YZ)) $ x
embed (XS (XS (XS (XZ x)))) = YS . fmap (YS . fmap (YS . fmap YZ)) $ x
-- embed (XS (XS (XS (XS x)))) = collapse . collapse . collapse . embed . XS $ x
embed (XS (XS (XS (XS x)))) = collapse . collapse . collapse . collapse . embed $ x

collapse :: forall f a. (Functor f) => Y f (f a) -> Y f a
collapse (YZ x) = YS . fmap YZ $ x
collapse (YS x) = YS . fmap collapse $ x
