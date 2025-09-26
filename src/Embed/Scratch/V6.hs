{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
module Embed.Scratch.V6 where

import Embed.Def

embed :: forall f a. (Functor f) => X f a -> Y f a
embed (XZ a) = YZ a
embed (XS x) = YS (distributeEmbed x)

distribute :: forall f a. (Functor f) => Y f (f a) -> f (Y f a)
distribute (YZ a) = fmap YZ . fmap id $ a
distribute (YS x) = fmap YS . fmap distribute $ x

distributeEmbed :: forall f a. (Functor f) => X f (f a) -> f (Y f a)
distributeEmbed (XZ a) = fmap YZ a
distributeEmbed (XS x) = distribute (YS (distributeEmbed x))
