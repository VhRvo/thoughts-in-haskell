{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use <$>" #-}
module Embed.Scratch.V7 where

import Embed.Def

embed :: forall f a. (Functor f) => X f a -> Y f a
embed (XZ a) = YZ a
embed (XS x) = YS (distributeEmbed x)

distribute :: forall f a. (Functor f) => Y f (f a) -> f (Y f a)
distribute (YZ a) = fmap YZ                   $ a
distribute (YS x) = fmap YS . fmap distribute $ x

distributeEmbed :: forall f a. (Functor f) => X f (f a) -> f (Y f a)
distributeEmbed (XZ a) = fmap YZ   a
distributeEmbed (XS x) = fmap YS . fmap distribute $ distributeEmbed @_ @(f a) x

-- >>> embed demoX
-- YS [YS [YZ 1],YS [YZ 2,YZ 3],YS [YZ 4,YZ 5,YZ 6]]

