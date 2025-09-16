{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use <$>" #-}
module Embed.Scratch.V72 where

import Embed.Def

embed :: forall f a. (Functor f) => X f a -> Y f a
embed (XZ a) = YZ a
embed (XS x) = YS (distributeEmbed x)

distribute :: forall f a. (Functor f) => Y f (f a) -> f (Y f a)
distribute (YZ a) = fmap YZ                $ a
distribute (YS x) = fmap (YS . distribute) $ x

distributeEmbed :: forall f a. (Functor f) => X f (f a) -> f (Y f a)
distributeEmbed (XZ a) = fmap YZ   a
-- distributeEmbed (XS x) = fmap (YS . distribute) $ distributeEmbed @_ @(f a) x
-- distributeEmbed (XS (XZ x)) = fmap (YS . distribute) . fmap YZ $ x
-- distributeEmbed (XS (XZ x)) = fmap (YS . distribute . YZ) $ x
distributeEmbed (XS (XZ x)) = fmap (YS . fmap YZ) $ x
-- distributeEmbed (XS (XS (XZ x))) = fmap (YS . fmap (YS . distribute) . distribute) . fmap YZ $ x
-- distributeEmbed (XS (XS (XZ x))) = fmap (YS . fmap (YS . distribute) . distribute . YZ) $ x
-- distributeEmbed (XS (XS (XZ x))) = fmap (YS . fmap (YS . distribute) . fmap YZ) $ x
-- distributeEmbed (XS (XS (XZ x))) = fmap (YS . fmap (YS . distribute . YZ)) $ x
distributeEmbed (XS (XS (XZ x))) = fmap (YS . fmap (YS . fmap YZ)) $ x
distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . fmap (YS . distribute) . distribute) . distribute) $ distributeEmbed x
