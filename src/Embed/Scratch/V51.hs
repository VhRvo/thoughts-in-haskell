{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant $" #-}
module Embed.Scratch.V51 where

import Embed.Def

-- embed :: forall f a. (Functor f) => X f a -> Y f a
-- embed (XZ a) = YZ a
-- embed (XS x) = distributeEmbed x

-- distribute :: forall f a. (Functor f) => Y f (f a) -> f (Y f a)
-- distribute (YZ a) = fmap YZ                $ a
-- distribute (YS x) = fmap (YS . distribute) $ x

-- distributeEmbed :: forall f a. (Functor f) => X f (f a) -> Y f a
-- distributeEmbed (XZ a) = fmap YZ a
-- distributeEmbed (XS x) = (YS . distribute) (distributeEmbed x)
